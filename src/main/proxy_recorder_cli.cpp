/*
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   Product name: redemption, a FLOSS RDP proxy
   Copyright (C) Wallix 2018
   Author(s): David Fort

   A proxy that will capture all the traffic to the target
*/

#include "proxy_recorder/proxy_recorder.hpp"

using PacketType = RecorderFile::PacketType;


/** @brief the server that handles RDP connections */
class FrontServer
{
public:
    FrontServer(std::string host, int port, std::string captureFile, std::string nla_username, std::string nla_password, bool enable_kerberos, bool forkable, uint64_t verbosity)
        : targetPort(port)
        , targetHost(std::move(host))
        , captureTemplate(std::move(captureFile))
        , nla_username(std::move(nla_username))
        , nla_password(std::move(nla_password))
        , enable_kerberos(enable_kerberos)
        , forkable(forkable)
        , verbosity(verbosity)
    {
        // just ignore this signal because there is no child termination management yet.
        struct sigaction sa;
        sa.sa_flags = 0;
        sigaddset(&sa.sa_mask, SIGCHLD);
        sa.sa_handler = SIG_IGN; /*NOLINT*/
        sigaction(SIGCHLD, &sa, nullptr);
    }

    bool start(int sck)
    {
        unique_fd sck_in {accept(sck, nullptr, nullptr)};
        if (!sck_in) {
            LOG(LOG_ERR, "Accept failed on socket %d (%s)", sck, strerror(errno));
            _exit(1);
        }

        const pid_t pid = this->forkable ? fork() : 0;
        connection_counter++;

        if(pid == 0) {
            close(sck);

            int nodelay = 1;
            if (setsockopt(sck_in.fd(), IPPROTO_TCP, TCP_NODELAY, &nodelay, sizeof(nodelay)) < 0) {
                LOG(LOG_ERR, "Failed to set socket TCP_NODELAY option on client socket");
                _exit(1);
            }

            char finalPathBuffer[256];
            char const* finalPath = captureTemplate.format(finalPathBuffer, connection_counter);
            LOG(LOG_INFO, "Recording front connection in %s", finalPath);

            TimeSystem timeobj;
            ProxyRecorder conn(
                std::move(sck_in), targetHost, targetPort, finalPath, timeobj,
                nla_username, nla_password, enable_kerberos, verbosity);
            try {
                conn.run();
            } catch(Error const& e) {
                if (errno) {
                    LOG(LOG_ERR, "Recording front connection ending: %s ; %s", e.errmsg(), strerror(errno));
                }
                else {
                    LOG(LOG_ERR, "Recording front connection ending: %s", e.errmsg());
                }
                exit(1);
            }
            exit(0);
        }
        else if (!this->forkable) {
            return false;
        }

        return true;
    }

private:
    struct CaptureTemplate
    {
        std::string captureTemplate;
        int startDigit = 0;
        int endDigit = 0;

        CaptureTemplate(std::string captureFile)
            : captureTemplate(std::move(captureFile))
        {
            std::string::size_type pos = 0;
            while ((pos = captureTemplate.find('%', pos)) != std::string::npos) {
                if (captureTemplate[pos+1] == 'd') {
                    startDigit = int(pos);
                    endDigit = startDigit + 2;
                    break;
                }
                ++pos;
            }
        }

        char const* format(array_view_char path, int counter) const
        {
            if (endDigit) {
                std::snprintf(path.data(), path.size(), "%.*s%04d%s",
                    startDigit, captureTemplate.c_str(),
                    counter,
                    captureTemplate.c_str() + endDigit
                );
                path.back() = '\0';
                return path.data();
            }

            return captureTemplate.c_str();
        }
    };

    int connection_counter = 0;
    int targetPort;
    std::string targetHost;
    CaptureTemplate captureTemplate;
    std::string nla_username;
    std::string nla_password;
    bool enable_kerberos;
    bool forkable;
    uint64_t verbosity;
};


struct CliPassword
{
    cli::Res operator()(cli::ParseResult& pr) const
    {
        if (!pr.str) {
            return cli::Res::BadFormat;
        }

        char* s = av[pr.opti];
        password = s;
        // hide password in /proc/...
        for (int i = 0; *s; ++s, ++i) {
            *s = (i < 3) ? '*' : '\0';
        }
        ++pr.opti;

        return cli::Res::Ok;
    }

    std::string& password;
    char ** av;
};

template<class Output, class Opt>
void print_action(Output&& out, Opt const& /*unused*/, CliPassword const& /*clipass*/)
{
    out << " [password]";
}

int main(int argc, char *argv[])
{
    char const* target_host = nullptr;
    int target_port = 3389;
    int listen_port = 3389;
    char const* capture_file = nullptr;
    std::string nla_username;
    std::string nla_password;
    bool no_forkable = false;
    bool enable_kerberos = false;
    uint64_t verbosity = 0;

    auto options = cli::options(
        cli::option('h', "help").help("Show help").action(cli::help),
        cli::option('v', "version").help("Show version")
            .action(cli::quit([]{ std::cout << "ProxyRecorder 1.0, " << redemption_info_version() << "\n"; })),
        cli::option('s', "target-host").action(cli::arg_location("host", target_host)),
        cli::option('p', "target-port").action(cli::arg_location("port", target_port)),
        cli::option('P', "port").help("Listen port").action(cli::arg_location(listen_port)),
        cli::option("nla-username").action(cli::arg_location("username", nla_username)),
        cli::option("nla-password").action(CliPassword{nla_password, argv}),
        cli::option("enable-kerberos").action(cli::on_off_location(enable_kerberos)),
        cli::option('t', "template").help("Ex: dump-%d.out")
            .action(cli::arg_location("path", capture_file)),
        cli::option('N', "no-fork").action(cli::on_off_location(no_forkable)),
        cli::option('V', "verbose").action(cli::arg_location("verbosity", verbosity))
    );

    auto cli_result = cli::parse(options, argc, argv);
    switch (cli_result.res) {
        case cli::Res::Ok:
            break;
        case cli::Res::Exit:
            return 0;
        case cli::Res::Help:
            cli::print_help(options, std::cout);
            return 0;
        case cli::Res::BadFormat:
        case cli::Res::BadOption:
            std::cerr << "Bad " << (cli_result.res == cli::Res::BadFormat ? "format" : "option") << " at parameter " << cli_result.opti;
            if (cli_result.opti < cli_result.argc) {
                std::cerr << " (" << cli_result.argv[cli_result.opti] << ")";
            }
            std::cerr << "\n";
            return 1;
    }

    if (!target_host) {
        std::cerr << "Missing --target-host\n";
    }

    if (!capture_file) {
        std::cerr << "Missing --template\n";
    }

    if (!target_host || !capture_file) {
        cli::print_help(options, std::cerr << "\n");
        return 1;
    }

    SSL_library_init();

    openlog("ProxyRecorder", LOG_CONS | LOG_PERROR, LOG_USER);

    FrontServer front(
        target_host, target_port, capture_file,
        std::move(nla_username), std::move(nla_password),
        enable_kerberos, !no_forkable, verbosity);
    auto sck = create_server(inet_addr("0.0.0.0"), listen_port);
    if (!sck) {
        return 2;
    }
    return unique_server_loop(std::move(sck), [&](int sck){
        return front.start(sck);
    });
}

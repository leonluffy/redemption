#define BOOST_AUTO_TEST_MAIN
#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MODULE TestVerifier
#include <boost/test/auto_unit_test.hpp>

#define LOGPRINT
// TODO
#define ENABLE_PROTO_TRACE
#if defined(ENABLE_PROTO_TRACE) && defined(NDEBUG)
# undef ENABLE_PROTO_TRACE
#endif

#include <iostream>
#include "proto/proto.hpp"

namespace XXX {
    PROTO_VAR(proto::types::u8, a);
    PROTO_VAR(proto::types::u8, b);
    PROTO_VAR(proto::types::bytes, c);
    PROTO_VAR(proto::types::u16_le, d);
    PROTO_VAR(proto::types::str8_to_str16, e);
    PROTO_VAR(proto::types::u16_encoding, f);

    constexpr auto desc = proto::desc(
        a, b, c, d, e, f,
        proto::sz<proto::types::u8>{},
        proto::sz_with_self<proto::types::u8>{}
    );
}

#include "core/RDP/sec.hpp"
#include "core/RDP/x224.hpp"

#include "proto/iovec.hpp"

void test_old();
void test_new();
void other_test();
void test();
void bench();

#include "proto/buffering_policy.hpp"
#include "proto/buffering2_policy.hpp"
#include "proto/buffering3_policy.hpp"

#include "utils/log.hpp" //hexdump_c

struct log_policy : buffering2_policy_base
{
    static void send(iovec_array iovs)
    {
        for (auto iov : iovs) {
            PROTO_TRACE(" [" << iov.iov_base << "] [len: " << iov.iov_len << "]\n");
            hexdump_c(static_cast<uint8_t const*>(iov.iov_base), iov.iov_len);
        }
    }
};

BOOST_AUTO_TEST_CASE(proto_test)
{
    struct {
        uint8_t a = 1;
        uint8_t b = 2;
        uint16_t d = 3;
        char const c[3] = "ab";
    } pkt;

    auto packet = XXX::desc(
        XXX::a = pkt.a,
        XXX::b = pkt.b,
        XXX::c = /*cstr_*/make_array_view(pkt.c),
        XXX::d = pkt.d,
        XXX::e = /*cstr_*/make_array_view(pkt.c),
        XXX::f = pkt.d/*,
//         XXX::sz,
        XXX::sz2
      , 1*/
    );

    packet.apply_for_each(Printer{});
    std::cout << "\n";
    packet.apply(Buffering{});
    std::cout << "\n";
    proto::apply(Buffering2<log_policy>{}, packet, packet);

    test();
    other_test();
    bench();
}


void test()
{
    std::cout << "\n\n======== old ========\n\n";
    test_old();
    std::cout << "\n\n======== new ========\n\n";
    test_new();
}


void test_old() {
    uint8_t data[10];
    CryptContext crypt;

    uint8_t buf[256];
    OutStream out_stream(buf + 126, 126);
    StaticOutStream<128> hstream;
    SEC::Sec_Send(out_stream, data, 10, ~SEC::SEC_ENCRYPT, crypt, 0);
    X224::DT_TPDU_Send(hstream, out_stream.get_offset());
    BOOST_REQUIRE_EQUAL(4, out_stream.get_offset());
    BOOST_REQUIRE_EQUAL(7, hstream.get_offset());
    auto p = out_stream.get_data() - hstream.get_offset();
    BOOST_REQUIRE_EQUAL(11, out_stream.get_current() - p);
    memcpy(p, hstream.get_data(), hstream.get_offset());
    out_stream = OutStream(p, out_stream.get_current() - p);
    out_stream.out_skip_bytes(out_stream.get_capacity());
    hexdump_c(out_stream.get_data(), out_stream.get_offset());
}

#include "utils/sugar/bytes_t.hpp"
inline bool check_range(const_bytes_array p, const_bytes_array mem, char * message)
{
    if (p.size() != mem.size() || memcmp(p.data(), mem.data(), p.size())) {
        if (auto len = p.size()) {
            auto sig = p.data();
            message += std::sprintf(message, "Expected signature: \"\\x%.2x", unsigned(*sig));
            while (--len) {
                message += std::sprintf(message, "\\x%.2x", unsigned(*++sig));
            }
            message[0] = '"';
            message[1] = 0;
        }
        message[0] = 0;
        return false;
    }
    return true;
}

#define CHECK_RANGE(p, mem)                      \
    {                                            \
        char message[1024*64];                   \
        if (!check_range(p, mem, message)) {     \
            BOOST_CHECK_MESSAGE(false, message); \
        }                                        \
    }

inline array_view_const_u8 iov2av(iovec const iov)
{
    return array_view_const_u8{
        reinterpret_cast<uint8_t const *>(iov.iov_base),
        iov.iov_len
    };
}

void test_new()
{
    auto packet1 = x224::dt_tpdu();

    CryptContext crypt;
    auto packet2 = sec::sec(
        sec::flags = decltype(SEC::SEC_ENCRYPT)(~SEC::SEC_ENCRYPT),
        sec::crypt = crypt
    );

    struct Policy : log_policy {
        void send(iovec_array iovs) const {
            BOOST_CHECK_EQUAL(iovs.size(), 1);
            CHECK_RANGE(
                iov2av(iovs[0]),
                cstr_array_view("\x03\x00\x00\x0b\x02\xf0\x80\xf7\xff\xff\xff")
            );
            log_policy::send(iovs);
            this->used = true;
        }

        Policy(bool & used) : used(used) {}
        bool & used;
    };

    bool used = false;
    proto::apply(Buffering2<Policy>{used}, packet1, packet2);
    BOOST_CHECK(used);


    struct Policy2 : log_policy {
        void send(array_view_u8 av) const {
            CHECK_RANGE(av, cstr_array_view("\x03\x00\x00\x0b\x02\xf0\x80\xf7\xff\xff\xff"));
            iovec iov{av.data(), av.size()};
            log_policy::send(iovec_array{&iov, 1u});
            this->used = true;
        }
        Policy2(bool & used) : used(used) {}
        bool & used;
    };

    uint8_t buf[1024];
    used = false;
    proto::apply(Buffering3<Policy2>{{used}, {buf}}, packet1, packet2);
    BOOST_CHECK(used);
}

struct lazy {
    proto::types::u8 a;

    using sizeof_ = proto::size_<2>;
    using is_reserializer = std::true_type;

    void static_reserialize(uint8_t * p, array_view_u8 /*av*/) const
    {
        p[0] = 15;
        p[1] = 15;
    }

    void static_reserialize(uint8_t * p, iovec_array /*av*/, std::size_t) const
    {
        p[0] = 15;
        p[1] = 15;
    }

    friend std::ostream & operator<<(std::ostream & os, lazy const & x)
    { return os << "lazy {a=" << int(x.a.val) << "}"; }
};

struct lazy2 {
    proto::types::u8 a;
    proto::types::u8 b;

    using sizeof_ = proto::size_<2>;

    void static_serialize(uint8_t * p) const
    {
        p[0] = 15;
        p[1] = 15;
    }

    friend std::ostream & operator<<(std::ostream & os, lazy2 const & x)
    { return os << "lazy {a=" << int(x.a.val) << "}"; }
};


void other_test()
{
    PROTO_VAR(proto::types::u8, a);
    PROTO_VAR(proto::types::u8, b);
    constexpr auto bl = proto::desc(
        proto::if_(a)
            [proto::composer(a, b)]
      , proto::if_(a)
            [a]

      , proto::if_(a)
            [b &= 1]
        .else_
            [b &= 1]

//         proto::if_(proto::params[a])
//             [proto::params[b] &= 1]
//         .elif_(proto::params[b])
//             [proto::params[b] &= 1]
//         .else_
//             [proto::params[b] &= 1]

        , proto::sz<proto::types::u8>{}
        , proto::creater<lazy>(a)
        , proto::creater<lazy2>(a, b)
        , proto::if_(a)
            [proto::sz<proto::types::u8>{}]
        , proto::retype<proto::types::u8>
            (a | a)
        , a |= a | a
        //, proto::params[a] |= proto::params[a] | proto::params[a]
        //, a |= proto::params[b]
    );

    uint8_t data[1024];
    struct Policy3 : log_policy {
        void send(array_view_u8 av) const {
            iovec iov{av.data(), av.size()};
            log_policy::send(iovec_array{&iov, 1u});
        }
    };
    proto::apply(
        Buffering3<Policy3>{{}, array_view_u8{data}},
        bl(a = 1_c, b = 3_c),
        proto::value(proto::types::u8{2_c}),
        proto::hook<class hook>([](auto...){}),
        proto::values(
            proto::types::u8{3_c},
            proto::types::u8{4_c},
            proto::types::bytes{cstr_array_view("")}
        )
    );

    std::cout << "\n\n";

    proto::apply(
        Buffering2<log_policy>{},
        bl(a = 1_c, b = 3_c),
        proto::value(proto::types::u8{2_c})
    );

    std::cout << "\n\n";

    PROTO_VAR(proto::types::u16_encoding, c);
    auto b2 = proto::desc(proto::sz<proto::types::u16_encoding>{});
    auto b3 = proto::desc(c);

    struct PolicyLL : log_policy {
        void send(iovec_array iovs) const {
            log_policy::send(iovs);
            BOOST_REQUIRE_EQUAL(iovs.size(), 2);
            BOOST_CHECK_EQUAL(iovs[0].iov_len, 1);
            BOOST_CHECK_EQUAL(iovs[1].iov_len, 2);
            CHECK_RANGE(
                iov2av(iovs[0]),
                cstr_array_view("\x02")
            );
            CHECK_RANGE(
                iov2av(iovs[1]),
                cstr_array_view("\x03\x02")
            );
            this->used = true;
        }

        PolicyLL(bool & used) : used(used) {}
        bool & used;
    };

    bool used = false;
    proto::apply(
        Buffering2<PolicyLL>{used},
        b2(),
        b3(c = 3_c),
        proto::value(proto::types::u8{2_c})
    );
    BOOST_CHECK(used);

    proto::apply(
        Buffering2<PolicyLL>{used},
        b2(),
        b3(c = 3_c),
        proto::hook<class hook>([](array_view_u8 av){
            using voidp = void*;
            std::cout << " [hook av{0x" << voidp(av.data()) << ", " << av.size() << "}] [data:";
            for (auto byte : av) {
                std::cout << " " << int(byte);
            }
            std::cout << "]";
            if (av.size() != 1) {
                std::cout << std::endl;
            }
            BOOST_CHECK_EQUAL(av.size(), 1);
        }, [](auto iovs, std::size_t total){
            std::cout << " [hook iovs.sz: " << iovs.size() << " total: " << total << "]" << std::endl;
            BOOST_CHECK(false);
        }),
        proto::value(proto::types::u8{2_c})
    );

    struct Policy2 : log_policy
    {
        void send(iovec_array iovs) const {
            log_policy::send(iovs);
            BOOST_REQUIRE_EQUAL(iovs.size(), 3);
            BOOST_CHECK_EQUAL(iovs[0].iov_len, 1);
            BOOST_CHECK_EQUAL(iovs[1].iov_len, 2);
            BOOST_CHECK_EQUAL(iovs[2].iov_len, 3);
            CHECK_RANGE(
                iov2av(iovs[0]),
                cstr_array_view("\x05")
            );
            CHECK_RANGE(
                iov2av(iovs[1]),
                cstr_array_view("\x03\x02")
            );
            CHECK_RANGE(
                iov2av(iovs[2]),
                cstr_array_view("abc")
            );
            this->used = true;
        }

        Policy2(bool & used) : used(used) {}
        bool & used;
    };
    proto::apply(
        Buffering2<Policy2>{used},
        b2(),
        b3(c = 3_c),
        proto::hook<class hook>([](array_view_u8 av){
            BOOST_CHECK(false);
            using voidp = void*;
            std::cout << " [hook av{0x" << voidp(av.data()) << ", " << av.size() << "}] [data:";
            BOOST_CHECK(false);
        }, [](iovec_array iovs, std::size_t total){
            std::cout << " [hook.sz: " << iovs.size() << "}] [data:";
            for (auto iov : iovs) {
                for (auto byte : array_view_u8{static_cast<uint8_t *>(iov.iov_base), iov.iov_len}) {
                    std::cout << " " << int(byte);
                }
            }
            std::cout << "]";
            if (iovs.size() != 2) {
                std::cout << std::endl;
            }
            BOOST_CHECK_EQUAL(iovs.size(), 2);
        }),
        proto::value(proto::types::u8{2_c}),
        proto::value(proto::types::bytes{{"abc", 3}})
    );
    BOOST_CHECK(used);

    struct Policy4 : log_policy
    {
        void send(iovec_array iovs) const {
            log_policy::send(iovs);
            BOOST_REQUIRE_EQUAL(iovs.size(), 3);
            BOOST_CHECK_EQUAL(iovs[0].iov_len, 1);
            BOOST_CHECK_EQUAL(iovs[1].iov_len, 1);
            BOOST_CHECK_EQUAL(iovs[2].iov_len, 3);
            CHECK_RANGE(
                iov2av(iovs[0]),
                cstr_array_view("\x04")
            );
            CHECK_RANGE(
                iov2av(iovs[1]),
                cstr_array_view("\x02")
            );
            CHECK_RANGE(
                iov2av(iovs[2]),
                cstr_array_view("abc")
            );
            this->used = true;
        }

        Policy4(bool & used) : used(used) {}
        bool & used;
    };
    proto::apply(
        Buffering2<Policy4>{used},
        proto::desc(
            proto::sz_with_self<proto::types::u16_encoding>{},
            c
        )(c = 2_c),
        proto::value(proto::types::bytes{{"abc", 3}})
    );
    BOOST_CHECK(used);
}

// #include <chrono>
//
// static void escape(void const * p) {
//    asm volatile("" : : "g"(p) : "memory");
// }
//
// static void clobber() {
//    asm volatile("" : : : "memory");
// }
//
//
//
// inline void test1(uint8_t * p, CryptContext & crypt, uint32_t c) {
//     auto packet1 = x224::dt_tpdu();
//     auto packet2 = sec::sec(
//         sec::flags = decltype(SEC::SEC_ENCRYPT)(c),
//         sec::crypt = crypt
//     );
//     struct Policy : buffering2_policy_base {
//         void send(iovec_array iovs) const {
//             //escape(data);
//             for (auto iovec : iovs) {
//                 memcpy(p, iovec.iov_base, iovec.iov_len);
//                 p += iovec.iov_len;
//             }
//             //clobber();
//         }
//         Policy(uint8_t * p) : p (p) {}
//         mutable uint8_t * p;
//     };
//     proto::apply(Buffering2<Policy>{p}, packet1, packet2);
// }
//
// inline void test2(uint8_t * p, CryptContext & crypt, uint32_t c) {
//     uint8_t data[10];
//     uint8_t buf[256];
//     OutStream out_stream(buf + 126, 126);
//     StaticOutStream<128> hstream;
// //         escape(out_stream.get_data());
// //         escape(hstream.get_data());
//     SEC::Sec_Send(out_stream, data, 10, c, crypt, 0);
//     X224::DT_TPDU_Send(hstream, out_stream.get_offset());
//     auto bufp = out_stream.get_data() - hstream.get_offset();
//     memcpy(bufp, hstream.get_data(), hstream.get_offset());
//     out_stream = OutStream(bufp, out_stream.get_current() - bufp);
//     out_stream.out_skip_bytes(out_stream.get_capacity());
//     memcpy(p, out_stream.get_data(), out_stream.get_offset());
// //         clobber();
// //         clobber();
// }
//
// #include "openssl_tls.hpp"

void bench()
{
//     SSL_load_error_strings();
//     SSL_library_init();
//
//     auto bench = [](auto test) {
//         std::vector<long long> v;
//
//         CryptContext crypt;
//         crypt.encryptionMethod = 1;
//         memcpy(crypt.key, "\xd1\x26\x9e\x63\xec\x51\x65\x1d\x89\x5c\x5a\x2a\x29\xef\x08\x4c", 16);
//         memcpy(crypt.update_key, crypt.key, 16);
//
//         crypt.rc4.set_key(crypt.key, (crypt.encryptionMethod==1)?8:16);
//
//         srand(0);
//
//         //unsigned imax = 0;
//         unsigned imax = 100;
//         //unsigned imax = 1;
//         //unsigned imax = 500;
//         for (unsigned i = 0; i < imax; ++i) {
//             //alignas(4) uint8_t data[2621];
//             alignas(4) uint8_t data[262144];
//             //alignas(4) uint8_t data[1048576];
//             auto p = data;
//             test(p, crypt, 0);
//             auto sz = 12;
//
//             using resolution_clock = std::chrono::steady_clock; // std::chrono::high_resolution_clock;
//
//             auto t1 = resolution_clock::now();
//
//             uint32_t r = rand();
//
//             while (static_cast<size_t>(p - data + sz) < sizeof(data)) {
//                 escape(p);
//                 test(p, crypt, r);
//                 clobber();
//                 p += sz;
//             }
//
//             auto t2 = resolution_clock::now();
//
//             v.push_back((t2-t1).count()/imax);
//         }
//         return v;
//     };
//
//      auto v1 = bench(test1);
//      auto v2 = bench(test2);
//
//      std::sort(v1.begin(), v1.end());
//      std::sort(v2.begin(), v2.end());
//
//      long long sz = v1.size();
//      auto pmin = std::max(sz/2-30, 0LL);
//      auto pmax = std::min(sz/2+29, sz);
//      v1 = decltype(v1)(std::begin(v1) + pmin, std::begin(v1) + pmax);
//      v2 = decltype(v2)(std::begin(v2) + pmin, std::begin(v2) + pmax);
//
//      std::cerr << "\n\ntest1\ttest2\n";
//      auto it1 = v1.begin();
//      for (auto t : v2) {
//          std::cerr
//            << *it1 << "\t" << t
//            << " \033[01;" << (*it1 < t ? "32m" : "31m") << std::showpos << (*it1 - t)
//            << std::noshowpos
//            << "  " << (*it1 * 100 / t) << "%"
//            << "\033[0m\n"
//         ;
//          ++it1;
//      }
}

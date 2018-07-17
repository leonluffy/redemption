/*
*   This program is free software; you can redistribute it and/or modify
*   it under the terms of the GNU General Public License as published by
*   the Free Software Foundation; either version 2 of the License, or
*   (at your option) any later version.
*
*   This program is distributed in the hope that it will be useful,
*   but WITHOUT ANY WARRANTY; without even the implied warranty of
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*   GNU General Public License for more details.
*
*   You should have received a copy of the GNU General Public License
*   along with this program; if not, write to the Free Software
*   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*
*   Product name: redemption, a FLOSS RDP proxy
*   Copyright (C) Wallix 2010-2017
*   Author(s): Clément Moroldo
*/

#include "utils/log.hpp"

#define RED_TEST_MODULE TestRDPMetrics
#include "system/redemption_unit_tests.hpp"

#include <sys/ioctl.h>
#include <sys/statvfs.h>
#include <linux/hdreg.h>

#include "utils/fileutils.hpp"
#include "utils/sugar/unique_fd.hpp"

#include "core/RDP/rdp_metrics.hpp"



RED_AUTO_TEST_CASE(TestRDPMetricsOutputFileTurnOver) {

    ClientInfo info;
    // tmp/rdp_metrics_file_test
    const char * templace_path_file = "tests/core/RDP/rdp_metrics_file_test";
    RDPMetrics metrics( templace_path_file
                      , 1
                      , "user"
                      , "admin"
                      , "10.10.13.12"
                      , info
                      , 0
                      , "RDP1");

    char current_date[24] {};
    timeval now = tvtime();
    metrics.set_current_formated_date(current_date, false, now.tv_sec);

    char complete_file_path[4096] = {'\0'};
    ::snprintf(complete_file_path, sizeof(complete_file_path), "%s-%s.log", templace_path_file, current_date);
    int fd = ::open(complete_file_path, O_RDONLY| O_APPEND);
    RED_CHECK(fd > 0);
    ::close(fd);
    remove(complete_file_path);

    time_t yesterday_time = metrics.last_date - 3600*24;
    metrics.last_date = yesterday_time;
    metrics.log();
    metrics.log();

    char yesterday_date[24] {};
    metrics.set_current_formated_date(yesterday_date, false, yesterday_time);
    char yesterday_complete_path[4096] = {'\0'};
    ::snprintf(yesterday_complete_path, sizeof(yesterday_complete_path), "%s-%s.log", templace_path_file, yesterday_date);
    fd = ::open(yesterday_complete_path, O_RDONLY| O_APPEND);
    RED_CHECK(fd == -1);
    RED_CHECK(yesterday_time <= metrics.last_date);

    fd = ::open(complete_file_path, O_RDONLY | O_APPEND);
    RED_CHECK(fd > 0);

    ::close(fd);
    remove(complete_file_path);
}


RED_AUTO_TEST_CASE(TestRDPMetricsOutputLogHeader) {

    ClientInfo info;
    const char * templace_path_file = "tests/core/RDP/rdp_metrics_file_test";
    RDPMetrics metrics( templace_path_file
                      , 1
                      , "user"
                      , "admin"
                      , "10.10.13.12"
                      , info
                      , 0
                      , "RDP1");

    char current_date[24] = {'\0'};
    timeval now = tvtime();
    metrics.set_current_formated_date(current_date, false, now.tv_sec);

    char complete_file_path[4096] = {'\0'};
    ::snprintf(complete_file_path, sizeof(complete_file_path), "%s-%s.log", templace_path_file, current_date);
    int fd = ::open(complete_file_path, O_RDONLY| O_APPEND);
    RED_CHECK(fd > 0);
    metrics.log();

    std::string expected_log("Session_starting_time=");
    expected_log += metrics.start_full_date_time;
    expected_log += " delta_time(s)=0 Session_id=1 user=D033E22AE348AEB5660 account=12DEA96FEC20593566A hostname=DA39A3EE5E6B4B0D325 target_service=EE5D8A196324C9649DC session_info=1709919F0A4B52AE2A7";

    char log_read[512] = {'\0'};
    ::read(fd, log_read, expected_log.length());
    std::string str_log_read(log_read);
    RED_CHECK_EQUAL(str_log_read, expected_log);
    ::close(fd);
    remove(complete_file_path);
}


RED_AUTO_TEST_CASE(TestRDPMetricsOutputData) {

    ClientInfo info;
    const char * templace_path_file = "tests/core/RDP/rdp_metrics_file_test";
    RDPMetrics metrics( templace_path_file
                      , 1
                      , "user"
                      , "admin"
                      , "10.10.13.12"
                      , info
                      , 0
                      , "RDP1");

    char current_date[24] = {'\0'};
    timeval now = tvtime();
    metrics.set_current_formated_date(current_date, false, now.tv_sec);

    char complete_file_path[4096] = {'\0'};
    ::snprintf(complete_file_path, sizeof(complete_file_path), "%s-%s.log", templace_path_file, current_date);
    int fd = ::open(complete_file_path, O_RDONLY| O_APPEND);
    RED_CHECK(fd > 0);

    for (int i = 0; i < 31; i++) {
        metrics.current_data[i] = i+1;
    }
    metrics.log();

    std::string expected_log_header("Session_starting_time=");
    expected_log_header += metrics.start_full_date_time;
    expected_log_header += " delta_time(s)=0 Session_id=1 user=D033E22AE348AEB5660 account=12DEA96FEC20593566A hostname=DA39A3EE5E6B4B0D325 target_service=EE5D8A196324C9649DC session_info=1709919F0A4B52AE2A7";
    RED_CHECK(fd > 0);
    char log_read[2048] = {'\0'};
    ::read(fd, log_read, 1016);
    RED_CHECK(fd > 0);
    std::string str_log_read(log_read);
    RED_CHECK(fd > 0);
    std::string str_log_data = str_log_read.substr(expected_log_header.length(), str_log_read.length());
    //LOG(LOG_INFO, "%s", str_log_data);

    std::string expected_log_data(" main_channel_data_from_client=1 right_click_sent=2 left_click_sent=3 keys_sent=4 mouse_move=5 main_channel_data_from_serveur=6 cliprdr_channel_data_from_server=7 nb_text_paste_server=8 nb_image_paste_server=9 nb_file_paste_server=10 nb_text_copy_server=11 nb_image_copy_server=12 nb_file_copy_server=13 cliprdr_channel_data_from_client=14 nb_text_paste_client=15 nb_image_paste_client=16 nb_file_paste_client=17 nb_text_copy_client=18 nb_image_copy_client=19 nb_file_copy_client=20 rdpdr_channel_data_from_client=21 rdpdr_channel_data_from_server=22 nb_more_1k_byte_read_file=23 nb_deleted_file_or_folder=24 nb_write_file=25 nb_rename_file=26 nb_open_folder=27 rail_channel_data_from_client=28 rail_channel_data_from_server=29 other_channel_data_from_client=30 other_channel_data_from_server=31");

    RED_CHECK_EQUAL(expected_log_data, str_log_data);
    ::close(fd);
    remove(complete_file_path);
}


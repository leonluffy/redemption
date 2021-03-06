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
   Copyright (C) Wallix 2012
   Author(s): Christophe Grosjean

   Unit test to capture interface to video recording to flv or mp4
*/

#include "test_only/test_framework/redemption_unit_tests.hpp"

#include "capture/video_capture.hpp"

#ifndef REDEMPTION_NO_FFMPEG

#include "core/RDP/orders/RDPOrdersPrimaryOpaqueRect.hpp"
#include "capture/full_video_params.hpp"
#include "capture/capture_params.hpp"
#include "core/RDP/RDPDrawable.hpp"
#include "utils/fileutils.hpp"


inline void simple_movie(
    timeval now, unsigned duration, RDPDrawable & drawable,
    gdi::CaptureApi & capture, bool ignore_frame_in_timeval, bool mouse
) {
    Rect screen(0, 0, drawable.width(), drawable.height());
    auto const color_cxt = gdi::ColorCtx::depth24();
    drawable.draw(RDPOpaqueRect(screen, encode_color24()(BLUE)), screen, color_cxt);

    uint64_t usec = now.tv_sec * 1000000LL + now.tv_usec;
    Rect r(10, 10, 50, 50);
    int vx = 5;
    int vy = 4;
    for (size_t x = 0; x < duration; x++) {
        drawable.draw(RDPOpaqueRect(r, encode_color24()(BLUE)), screen, color_cxt);
        r.y += vy;
        r.x += vx;
        drawable.draw(RDPOpaqueRect(r, encode_color24()(WABGREEN)), screen, color_cxt);
        usec += 40000LL;
        now.tv_sec  = usec / 1000000LL;
        now.tv_usec = (usec % 1000000LL);
        //printf("now sec=%u usec=%u\n", (unsigned)now.tv_sec, (unsigned)now.tv_usec);
        int cursor_x = mouse?r.x + 10:0;
        int cursor_y = mouse?r.y + 10:0;
        drawable.set_mouse_cursor_pos(cursor_x, cursor_y);
        capture.periodic_snapshot(now, cursor_x, cursor_y, ignore_frame_in_timeval);
        capture.periodic_snapshot(now, cursor_x, cursor_y, ignore_frame_in_timeval);
        if ((r.x + r.cx >= drawable.width())  || (r.x < 0)) { vx = -vx; }
        if ((r.y + r.cy >= drawable.height()) || (r.y < 0)) { vy = -vy; }
    }
    // last frame (video.encoding_video_frame())
    usec += 40000LL;
    now.tv_sec  = usec / 1000000LL;
    now.tv_usec = (usec % 1000000LL);
    int cursor_x = mouse?r.x + 10:0;
    int cursor_y = mouse?r.y + 10:0;
    capture.periodic_snapshot(now, cursor_x, cursor_y, ignore_frame_in_timeval);
}

RED_AUTO_TEST_CASE(TestSequencedVideoCapture)
{
    {
        struct notified_on_video_change : public NotifyNextVideo
        {
            void notify_next_video(const timeval& /*now*/, reason /*reason*/) override
            {
                // TEST test
            }
        } next_video_notifier;

        timeval now; now.tv_sec = 1353055800; now.tv_usec = 0;
        RDPDrawable drawable(800, 600);
        VideoParams video_params{Level::high, drawable.width(), drawable.height(), 25, 15, 100000, "flv", false, false, false, std::chrono::microseconds{2 * 1000000l}, 0};
        CaptureParams capture_params{
            now, "opaquerect_videocapture", nullptr, "./", 0 /* groupid */, nullptr, SmartVideoCropping::disable, 0};
        SequencedVideoCaptureImpl video_capture(
            capture_params, 100 /* zoom */, drawable, drawable, video_params,
            next_video_notifier);
        simple_movie(now, 250, drawable, video_capture, false, true);
    }

    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_videocapture-000000.png", 3099);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_videocapture-000000.flv", 77155);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_videocapture-000001.png", 3104);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_videocapture-000001.flv", 75432);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_videocapture-000002.png", 3107);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_videocapture-000002.flv", 77291);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_videocapture-000003.png", 3099);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_videocapture-000003.flv", 76735);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_videocapture-000004.png", 3098);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_videocapture-000004.flv", 75604);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_videocapture-000005.png", 3098);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_videocapture-000005.flv", 18741);
}

RED_AUTO_TEST_CASE(TestSequencedVideoCaptureMP4)
{
    {
        struct notified_on_video_change : public NotifyNextVideo
        {
            void notify_next_video(const timeval& /*now*/, reason /*reason*/) override
            {
                // TEST test
            }
        } next_video_notifier;

        timeval now; now.tv_sec = 1353055800; now.tv_usec = 0;
        RDPDrawable drawable(800, 600);
        VideoParams video_params{Level::high, drawable.width(), drawable.height(), 25, 15, 100000, "mp4", false, false, false, std::chrono::microseconds{2 * 1000000l}, 0};
        CaptureParams capture_params{
            now, "opaquerect_videocapture", nullptr, "./", 0 /* groupid */, nullptr, SmartVideoCropping::disable, 0};
        SequencedVideoCaptureImpl video_capture(
            capture_params, 100 /* zoom */, drawable, drawable, video_params, next_video_notifier);
        simple_movie(now, 250, drawable, video_capture, false, true);
    }

    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_videocapture-000000.png", 3099);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_videocapture-000000.mp4", 25467 +- 200_v);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_videocapture-000001.png", 3104);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_videocapture-000001.mp4", 25016 +- 200_v);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_videocapture-000002.png", 3107);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_videocapture-000002.mp4", 25192 +- 200_v);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_videocapture-000003.png", 3099);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_videocapture-000003.mp4", 24640 +- 200_v);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_videocapture-000004.png", 3098);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_videocapture-000004.mp4", 24409 +- 200_v);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_videocapture-000005.png", 3098);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_videocapture-000005.mp4", 6618 +- 200_v);
}

RED_AUTO_TEST_CASE(TestVideoCaptureOneChunkFLV)
{
    struct notified_on_video_change : public NotifyNextVideo
    {
        void notify_next_video(const timeval& /*now*/, reason /*reason*/) override
        {
            // TEST
        }
    } next_video_notifier;

    {
        timeval now; now.tv_sec = 1353055800; now.tv_usec = 0;
        RDPDrawable drawable(800, 600);
        VideoParams video_params{Level::high, drawable.width(), drawable.height(), 25, 15, 100000, "flv", false, false, false, std::chrono::microseconds{1000 * 1000000l}, 0};
        CaptureParams capture_params{
            now, "opaquerect_videocapture_one_chunk_xxx", nullptr, "./", 0 /* groupid */, nullptr, SmartVideoCropping::disable, 0};
        SequencedVideoCaptureImpl video_capture(
            capture_params, 100 /* zoom */, drawable, drawable, video_params, next_video_notifier);
        simple_movie(now, 1000, drawable, video_capture, false, true);
    }

    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_videocapture_one_chunk_xxx-000000.png", 3099);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_videocapture_one_chunk_xxx-000000.flv", 1185483);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_videocapture_one_chunk_xxx-000001.png", -1);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_videocapture_one_chunk_xxx-000001.flv", -1);
}

RED_AUTO_TEST_CASE(TestFullVideoCaptureFlv)
{
    {
        timeval now; now.tv_sec = 1353055800; now.tv_usec = 0;
        RDPDrawable drawable(800, 600);
        VideoParams video_params{Level::high, drawable.width(), drawable.height(), 25, 15, 100000, "flv", false, false, false, {}, 0};
        CaptureParams capture_params{
            now, "opaquerect_fullvideocapture_timestamp1", nullptr, "./", 0 /* groupid */, nullptr, SmartVideoCropping::disable, 0};
        FullVideoCaptureImpl video_capture(
            capture_params, drawable, drawable, video_params, FullVideoParams{false});
        simple_movie(now, 250, drawable, video_capture, false, true);
    }

    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_fullvideocapture_timestamp1.flv", 307698 +- 15000_v);
}

RED_AUTO_TEST_CASE(TestFullVideoCaptureFlv2)
{
    {
        timeval now; now.tv_sec = 1353055800; now.tv_usec = 0;
        RDPDrawable drawable(800, 600);
        VideoParams video_params{Level::high, drawable.width(), drawable.height(), 25, 15, 100000, "flv", false, false, false, {}, 0};
        CaptureParams capture_params{
            now, "opaquerect_fullvideocapture_timestamp_mouse0", nullptr, "./", 0 /* groupid */, nullptr, SmartVideoCropping::disable, 0};
        FullVideoCaptureImpl video_capture(
            capture_params, drawable, drawable, video_params, FullVideoParams{false});
        simple_movie(now, 250, drawable, video_capture, false, false);
    }
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_fullvideocapture_timestamp_mouse0.flv", 298467 +- 5000_v);
}

RED_AUTO_TEST_CASE(TestFullVideoCaptureX264)
{
    {
        timeval now; now.tv_sec = 1353055800; now.tv_usec = 0;
        RDPDrawable drawable(800, 600);
        VideoParams video_params{Level::high, drawable.width(), drawable.height(), 25, 15, 100000, "mp4", false, false, false, {}, 0};
        CaptureParams capture_params{
            now, "opaquerect_fullvideocapture_timestamp2", nullptr, "./", 0 /* groupid */, nullptr, SmartVideoCropping::disable, 0};
        FullVideoCaptureImpl video_capture(
            capture_params, drawable, drawable, video_params, FullVideoParams{false});
        simple_movie(now, 250, drawable, video_capture, false, true);
    }
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_fullvideocapture_timestamp2.mp4", 123987 +- 300_v);
}

RED_AUTO_TEST_CASE(SequencedVideoCaptureFLV)
{
    struct notified_on_video_change : public NotifyNextVideo
    {
        void notify_next_video(const timeval& /*now*/, reason /*reason*/) override
        {
            // TEST check notification
        }
    } next_video_notifier;

    {
        timeval now; now.tv_sec = 1353055800; now.tv_usec = 0;
        RDPDrawable drawable(800, 600);
        VideoParams video_params{Level::high, drawable.width(), drawable.height(), 25, 15, 100000, "flv", false, false, false, std::chrono::microseconds{1000000}, 0};
        CaptureParams capture_params{
            now, "opaquerect_seqvideocapture", nullptr, "./", 0 /* groupid */, nullptr, SmartVideoCropping::disable, 0};
        SequencedVideoCaptureImpl video_capture(
            capture_params, 100 /* zoom */, drawable, drawable, video_params, next_video_notifier);
        simple_movie(now, 250, drawable, video_capture, false, true);
    }

    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture-000000.png", 3099);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture-000000.flv", 47215);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture-000001.png", 3099);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture-000001.flv", 48300);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture-000002.png", 3104);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture-000002.flv", 46964);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture-000003.png", 3101);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture-000003.flv", 46982);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture-000004.png", 3107);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture-000004.flv", 47031);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture-000005.png", 3101);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture-000005.flv", 48723);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture-000006.png", 3099);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture-000006.flv", 46847);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture-000007.png", 3101);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture-000007.flv", 48287);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture-000008.png", 3098);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture-000008.flv", 46747);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture-000009.png", 3098);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture-000009.flv", 47214);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture-000010.png", 3098);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture-000010.flv", 18741);
}


RED_AUTO_TEST_CASE(SequencedVideoCaptureX264)
{
    struct notified_on_video_change : public NotifyNextVideo
    {
        void notify_next_video(const timeval& /*now*/, reason /*reason*/) override
        {
            // TEST check notification
        }
    } next_video_notifier;

    {
        timeval now; now.tv_sec = 1353055800; now.tv_usec = 0;
        RDPDrawable drawable(800, 600);
        VideoParams video_params{Level::high, drawable.width(), drawable.height(), 25, 15, 100000, "mp4", false, false, false, std::chrono::microseconds{1000000}, 0};
        CaptureParams capture_params{
            now, "opaquerect_seqvideocapture_timestamp2", nullptr, "./", 0 /* groupid */, nullptr, SmartVideoCropping::disable, 0};
        SequencedVideoCaptureImpl video_capture(
            capture_params, 100 /* zoom */, drawable, drawable, video_params, next_video_notifier);
        simple_movie(now, 250, drawable, video_capture, false, true);
    }

    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture_timestamp2-000000.png", 3099);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture_timestamp2-000000.mp4", 13477 +- 100_v);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture_timestamp2-000001.png", 3099);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture_timestamp2-000001.mp4", 13470 +- 100_v);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture_timestamp2-000002.png", 3104);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture_timestamp2-000002.mp4", 13299 +- 100_v);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture_timestamp2-000003.png", 3101);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture_timestamp2-000003.mp4", 13196 +- 100_v);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture_timestamp2-000004.png", 3107);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture_timestamp2-000004.mp4", 13212 +- 100_v);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture_timestamp2-000005.png", 3101);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture_timestamp2-000005.mp4", 13457 +- 100_v);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture_timestamp2-000006.png", 3099);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture_timestamp2-000006.mp4", 13054 +- 100_v);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture_timestamp2-000007.png", 3101);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture_timestamp2-000007.mp4", 13064 +- 100_v);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture_timestamp2-000008.png", 3098);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture_timestamp2-000008.mp4", 12903 +- 100_v);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture_timestamp2-000009.png", 3098);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture_timestamp2-000009.mp4", 12983 +- 100_v);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture_timestamp2-000010.png", 3098);
    RED_CHECK_FILE_SIZE_AND_CLEAN("./opaquerect_seqvideocapture_timestamp2-000010.mp4", 6618 +- 50_v);
}
#else
RED_AUTO_TEST_CASE(NoTest)
{}
#endif

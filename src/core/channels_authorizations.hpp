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
 *   Copyright (C) Wallix 2010-2014
 *   Author(s): Christophe Grosjean, Raphael Zhou, Jonathan Poelen, Meng Tan
 */

#pragma once

#include "core/RDP/channels/rdpdr.hpp"
#include "core/channel_names.hpp"
#include "utils/sugar/array_view.hpp"
#include "utils/sugar/std_stream_proto.hpp"
#include "utils/sugar/movable_noncopyable.hpp"
#include "core/clipboard_virtual_channels_params.hpp"

#include <vector>
#include <string>
#include <array>


class ChannelsAuthorizations
{
public:
    ChannelsAuthorizations() = default;

    ChannelsAuthorizations(std::string const & allow, std::string const & deny);

    bool is_authorized(CHANNELS::ChannelNameId id) const noexcept;

    bool rdpdr_type_all_is_authorized() const noexcept;

    bool rdpdr_type_is_authorized(rdpdr::RDPDR_DTYP DeviceType) const noexcept;

    bool rdpdr_drive_read_is_authorized() const noexcept;
    bool rdpdr_drive_write_is_authorized() const noexcept;

    bool rdpsnd_audio_output_is_authorized() const noexcept;

    REDEMPTION_FRIEND_OSTREAM(out, ChannelsAuthorizations const & auth);

    static void update_authorized_channels(
        std::string & allow, std::string & deny, const std::string & proxy_opt
    );

    const ClipboardVirtualChannelParams get_clipboard_virtual_channel_params(bool disable_clipboard_log_syslog, bool disable_clipboard_log_wrm, bool log_only_relevant_clipboard_activities ) const
    {
        ClipboardVirtualChannelParams cvc_params;
        cvc_params.clipboard_down_authorized = this->cliprdr_down_is_authorized();
        cvc_params.clipboard_up_authorized   = this->cliprdr_up_is_authorized();
        cvc_params.clipboard_file_authorized = this->cliprdr_file_is_authorized();
        cvc_params.dont_log_data_into_syslog = disable_clipboard_log_syslog;
        cvc_params.dont_log_data_into_wrm    = disable_clipboard_log_wrm;
        cvc_params.log_only_relevant_clipboard_activities = log_only_relevant_clipboard_activities;
        return cvc_params;
    }

private:

    bool cliprdr_up_is_authorized() const noexcept;
    bool cliprdr_down_is_authorized() const noexcept;
    bool cliprdr_file_is_authorized() const noexcept;

    static constexpr const std::array<array_view_const_char, 3> cliprde_list()
    {
        return {{
            cstr_array_view("cliprdr_up,"),
            cstr_array_view("cliprdr_down,"),
            cstr_array_view("cliprdr_file,"),
        }};
    }
    static constexpr const std::array<array_view_const_char, 5> rdpdr_list()
    {
        return {{
            cstr_array_view("rdpdr_printer,"),
            cstr_array_view("rdpdr_port,"),
            cstr_array_view("rdpdr_drive_read,"),
            cstr_array_view("rdpdr_drive_write,"),
            cstr_array_view("rdpdr_smartcard,"),
        }};
    }
    static constexpr const std::array<array_view_const_char, 1> rdpsnd_list()
    {
        return {{
            cstr_array_view("rdpsnd_audio_output,"),
        }};
    }

    array_view<CHANNELS::ChannelNameId const> rng_allow() const;
    array_view<CHANNELS::ChannelNameId const> rng_deny() const;

    // Boolean structures moved around in other parts of the code
    // could merely be restricted to what we have below
    // See equivalent fields in : core/file_system_virtual_channel_params.hpp
    // and core/clipboard_virtual_channels_params.hpp
//    struct {
//        bool up;    // client to server
//        bool down;  // server to client
//        bool file;  // file copy
//    } rdpsnd_acl;

//    struct {
//        bool up;    // client to server
//        bool down;  // server to client
//        bool file;  // file copy
//    } cliprdr_acl;

//    struct {
//        bool printer;
//        bool port;
//        bool drive_read;
//        bool drive_write;
//        bool smartcard;
//    } rdpdr_acl;

    std::vector<CHANNELS::ChannelNameId> allow_and_deny_;
    size_t allow_and_deny_pivot_;
    bool all_allow_ = false;
    bool all_deny_ = false;
    // TODO array<bool, n> -> bitset<n> | uint8_t | enum
    std::array<bool, decltype(rdpdr_list())().size()> rdpdr_restriction_ {{}};
    std::array<bool, decltype(cliprde_list())().size()> cliprdr_restriction_ {{}};
    std::array<bool, decltype(rdpsnd_list())().size()> rdpsnd_restriction_ {{}};
};

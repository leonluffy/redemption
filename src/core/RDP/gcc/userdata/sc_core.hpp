/*
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   Product name: redemption, a FLOSS RDP proxy
   Copyright (C) Wallix 2016
   Author(s): Christophe Grosjean

   Generic Conference Control (T.124)

   T.124 GCC is defined in:

   http://www.itu.int/rec/T-REC-T.124-199802-S/en
   ITU-T T.124 (02/98): Generic Conference Control

*/

#pragma once

#include "core/RDP/gcc/data_block_type.hpp"
#include "utils/stream.hpp"
#include "core/error.hpp"

// 2.2.1.4.2 Server Core Data (TS_UD_SC_CORE)
// ==========================================

// The TS_UD_SC_CORE data block contains core server connection-related
// information.

// header (4 bytes): GCC user data block header, as specified in User Data
//  Header (section 2.2.1.3.1). The User Data Header type field MUST be set to
//  SC_CORE (0x0C01).

// version (4 bytes): A 32-bit, unsigned integer. The server version number for
//  the RDP. The major version number is stored in the high two bytes, while the
//  minor version number is stored in the low two bytes.

// 0x00080001 RDP 4.0 servers
// 0x00080004 RDP 5.0, 5.1, 5.2, 6.0, 6.1, 7.0, 7.1 and 8.0 servers

// If the server advertises a version number greater than or equal to 0x00080004,
// it MUST support a maximum length of 512 bytes for the UserName field in the
// Info Packet (section 2.2.1.11.1.1).

// clientRequestedProtocols (4 bytes): A 32-bit, unsigned integer that contains
// the flags sent by the client in the requestedProtocols field of the RDP
// Negotiation Request (section 2.2.1.1.1). In the event that an RDP Negotiation
// Request was not received from the client, this field MUST be initialized to
// PROTOCOL_RDP (0). If this field is not present, all of the subsequent
// fields MUST NOT be present.

// earlyCapabilityFlags (4 bytes): A 32-bit, unsigned integer that specifies
// capabilities early in the connection sequence. If this field is present, all
// of the preceding fields MUST also be present.

// +----------------------------------+-----------------------------------------+
// |             0x00000001           | Indicates that the following key        |
// | RNS_UD_SC_EDGE_ACTIONS_SUPPORTED | combinations are reserved by the server |
// |                                  | operating system:                       |
// |                                  |             - WIN + Z                   |
// |                                  |             - WIN + CTRL + TAB          |
// |                                  |             - WIN + C                   |
// |                                  |             - WIN + .                   |
// |                                  |             - WIN + SHIFT + .           |
// |                                  | In addition, the monitor boundaries of  |
// |                                  | the remote session are employed by the  |
// |                                  | server operating system to trigger user |
// |                                  | interface elements via touch or mouse   |
// |                                  | gestures.                               |
// +----------------------------------+-----------------------------------------+
// |             0x00000002           | Indicates that the server supports      |
// | RNS_UD_SC_DYNAMIC_DST_SUPPORTED  | Dynamic DST. Dynamic DST information is |
// |                                  | provided by the client in the           |
// |                                  | cbDynamicDSTTimeZoneKeyName,            |
// |                                  | dynamicDSTTimeZoneKeyName and           |
// |                                  | dynamicDaylightTimeDisabled fields of   |
// |                                  | the Extended Info Packet (section       |
// |                                  | 2.2.1.11.1.1.1).                        |
// +----------------------------------+-----------------------------------------+

// Exemple:
//01 0c 0c 00 -> TS_UD_HEADER::type = SC_CORE (0x0c01), length = 12 bytes
//04 00 08 00 -> TS_UD_SC_CORE::version = 0x0080004
//00 00 00 00 -> TS_UD_SC_CORE::clientRequestedProtocols = PROTOCOL_RDP
namespace GCC { namespace UserData {
// 2.2.1.4.2 Server Core Data (TS_UD_SC_CORE)
// ==========================================

// The TS_UD_SC_CORE data block contains core server connection-related
// information.

// header (4 bytes): GCC user data block header, as specified in User Data
//  Header (section 2.2.1.3.1). The User Data Header type field MUST be set to
//  SC_CORE (0x0C01).

// version (4 bytes): A 32-bit, unsigned integer. The server version number for
//  the RDP. The major version number is stored in the high two bytes, while the
//  minor version number is stored in the low two bytes.

// 0x00080001 RDP 4.0 servers
// 0x00080004 RDP 5.0, 5.1, 5.2, 6.0, 6.1, 7.0, 7.1 and 8.0 servers

// If the server advertises a version number greater than or equal to 0x00080004,
// it MUST support a maximum length of 512 bytes for the UserName field in the
// Info Packet (section 2.2.1.11.1.1).

// clientRequestedProtocols (4 bytes): A 32-bit, unsigned integer that contains
// the flags sent by the client in the requestedProtocols field of the RDP
// Negotiation Request (section 2.2.1.1.1). In the event that an RDP Negotiation
// Request was not received from the client, this field MUST be initialized to
// PROTOCOL_RDP (0). If this field is not present, all of the subsequent
// fields MUST NOT be present.

// earlyCapabilityFlags (4 bytes): A 32-bit, unsigned integer that specifies
// capabilities early in the connection sequence. If this field is present, all
// of the preceding fields MUST also be present.

// +----------------------------------+-----------------------------------------+
// |             0x00000001           | Indicates that the following key        |
// | RNS_UD_SC_EDGE_ACTIONS_SUPPORTED | combinations are reserved by the server |
// |                                  | operating system:                       |
// |                                  |             - WIN + Z                   |
// |                                  |             - WIN + CTRL + TAB          |
// |                                  |             - WIN + C                   |
// |                                  |             - WIN + .                   |
// |                                  |             - WIN + SHIFT + .           |
// |                                  | In addition, the monitor boundaries of  |
// |                                  | the remote session are employed by the  |
// |                                  | server operating system to trigger user |
// |                                  | interface elements via touch or mouse   |
// |                                  | gestures.                               |
// +----------------------------------+-----------------------------------------+
// |             0x00000002           | Indicates that the server supports      |
// | RNS_UD_SC_DYNAMIC_DST_SUPPORTED  | Dynamic DST. Dynamic DST information is |
// |                                  | provided by the client in the           |
// |                                  | cbDynamicDSTTimeZoneKeyName,            |
// |                                  | dynamicDSTTimeZoneKeyName and           |
// |                                  | dynamicDaylightTimeDisabled fields of   |
// |                                  | the Extended Info Packet (section       |
// |                                  | 2.2.1.11.1.1.1).                        |
// +----------------------------------+-----------------------------------------+

// Exemple:
//01 0c 0c 00 -> TS_UD_HEADER::type = SC_CORE (0x0c01), length = 12 bytes
//04 00 08 00 -> TS_UD_SC_CORE::version = 0x0080004
//00 00 00 00 -> TS_UD_SC_CORE::clientRequestedProtocols = PROTOCOL_RDP

struct SCCore {
    uint16_t userDataType;
    uint16_t length;
    uint32_t version;
    uint32_t clientRequestedProtocols;
    uint32_t earlyCapabilityFlags;

    SCCore()
    : userDataType(SC_CORE)
    , length(8)
    , version(0x00080001)
    , clientRequestedProtocols(0)
    , earlyCapabilityFlags(0)
    {
    }

    void emit(OutStream & stream)
    {
        if (this->length != 8
        && this->length != 12
        && this->length != 16) {
            LOG(LOG_ERR, "SC_CORE invalid length (%u)", this->length);
            throw Error(ERR_GCC);
        };

        stream.out_uint16_le(this->userDataType);
        stream.out_uint16_le(this->length);
        stream.out_uint32_le(this->version);

        if (this->length >= 12){
            stream.out_uint32_le(this->clientRequestedProtocols);
        }
        if (this->length >= 16){
            stream.out_uint32_le(this->earlyCapabilityFlags);
        }
    }

    void recv(InStream & stream)
    {
        if (!stream.in_check_rem(8)){
            LOG(LOG_ERR, "SC_CORE short header");
            throw Error(ERR_GCC);
        }

        this->userDataType = stream.in_uint16_le();
        this->length = stream.in_uint16_le();
        this->version = stream.in_uint32_le();
        if (this->length < 12) {
            if (this->length != 8) {
                LOG(LOG_ERR, "SC_CORE invalid length (%u)", this->length);
                throw Error(ERR_GCC);
            }
            return;
        }
        this->clientRequestedProtocols = stream.in_uint32_le();
        if (this->length < 16) {
            if (this->length != 12) {
                LOG(LOG_ERR, "SC_CORE invalid length (%u)", this->length);
                throw Error(ERR_GCC);
            }
            return;
        }
        this->earlyCapabilityFlags = stream.in_uint32_le();
        if (this->length != 16) {
            LOG(LOG_ERR, "SC_CORE invalid length (%u)", this->length);
            throw Error(ERR_GCC);
        }
    }

    void log(const char * msg)
    {
        // --------------------- Base Fields ---------------------------------------
        LOG(LOG_INFO, "%s GCC User Data SC_CORE (%u bytes)", msg, this->length);
        LOG(LOG_INFO, "sc_core::version [%04x] %s", this->version,
              (this->version==0x00080001) ? "RDP 4 client"
             :(this->version==0x00080004) ? "RDP 5.0, 5.1, 5.2, 6.0, 6.1, 7.0, 7.1 and 8.0 servers)"
                                          : "Unknown client");
        if (this->length < 12) {
            if (this->length != 8) {
                LOG(LOG_ERR, "SC_CORE invalid length (%u)", this->length);
                throw Error(ERR_GCC);
            }
            return;
        }
        LOG(LOG_INFO, "sc_core::clientRequestedProtocols  = %u", this->clientRequestedProtocols);
        if (this->length < 16) {
            if (this->length != 12) {
                LOG(LOG_ERR, "SC_CORE invalid length (%u)", this->length);
                throw Error(ERR_GCC);
            }
            return;
        }
        LOG(LOG_INFO, "sc_core::earlyCapabilityFlags  = %u", this->earlyCapabilityFlags);
        if (this->length != 16) {
            LOG(LOG_ERR, "SC_CORE invalid length (%u)", this->length);
            throw Error(ERR_GCC);
        }
    }
};

}}

// #include "proto/proto.hpp"
// #include "falcon/literals/integer_constant.hpp"
// using namespace falcon::literals::integer_constant_literals;
// namespace gcc { namespace user_data
// {
//     PROTO_VAR(proto::types::u16_le, version);
//     PROTO_VAR(proto::types::u32_le, client_requested_protocols);
//     PROTO_VAR(proto::types::u32_le, early_capability_flags);
//     PROTO_VAR(proto::types::enum_u16_le<DATA_BLOCK_TYPE>, user_data_type);
//
//     constexpr auto sc_core = proto::desc(
//         user_data_type = SC_CORE,
//         proto::optseq_size() + 8_c,
//         version,
//         proto::optseq(client_requested_protocols, early_capability_flags)
//     );
//
//     PROTO_VAR(proto::types::u16_le, mcs_channel_id);
//     PROTO_VAR(proto::types::aligned32_size_and_range_u16_le<proto::types::u16_le>, channel_id_list);
//
//     constexpr auto sc_net = proto::desc(
//         user_data_type = SC_NET,
//         proto::pkt_sz<proto::types::u16_le>{},
//         mcs_channel_id,
//         channel_id_list
//     );
//
//     namespace server_proprietary_certificate
//     {
//         PROTO_VAR(proto::types::u32_le, dw_sig_alg_id);
//         PROTO_VAR(proto::types::u32_le, dw_key_alg_id);
//         PROTO_VAR(proto::types::u16_le, w_public_key_blob_type);
//         PROTO_VAR(proto::types::u16_le, w_public_key_blob_len); // rsapk size
//
//         namespace rsapk
//         {
//             PROTO_VAR(proto::types::u32_le, magic);
//             PROTO_VAR(proto::types::u32_le, key_len);
//             PROTO_VAR(proto::types::u32_le, bit_len);
//             PROTO_VAR(proto::types::u32_le, data_len);
//             PROTO_VAR(proto::types::array_u8<4>, pub_exp);
//             PROTO_VAR(proto::types::array_u8<64>, modulus);
//         }
//
//         PROTO_VAR(proto::types::u16_le, w_signature_blob_type);
//         PROTO_VAR(proto::types::size_u16_le_and_array_u8<64>, w_signature_blob);
//     }
//
//     PROTO_VAR(proto::types::u32_le, encryption_method);
//     PROTO_VAR(proto::types::u32_le, encryption_level);
//     PROTO_VAR(proto::types::u32_le, server_random_len);
//     PROTO_VAR(proto::types::u32_le, server_cert_len);
//     PROTO_VAR(proto::types::array_u8<SEC_RANDOM_SIZE>, server_random);
//     PROTO_VAR(proto::types::u32_le, dw_version);
//     PROTO_VAR(proto::types::bool_, temporary);
//
//     constexpr auto sc_security_without_encryption = proto::desc(
//         user_data_type = SC_SECURITY,
//         proto::val<class length, proto::types::u16_le>{{0_c}},
//         encryption_method = 0_c,
//         encryption_level = 0_c
//     );
//
//     constexpr auto sc_security_with_encryption = proto::desc(
//         user_data_type = SC_SECURITY,
//         proto::val<class length, proto::types::u16_le>{{236_c}},
//         encryption_method,
//         encryption_level,
//         proto::val<class server_random_size, proto::types::u32_le>{{server_random.opti_size()}},
//         proto::val<class server_cert_len, proto::types::u32_le>{{184_c}} /* len of rsa info(certificate) */,
//         server_random,
//         /* start certificate */
//         dw_version |= temporary << 31,
//         server_proprietary_certificate::dw_sig_alg_id = SIGNATURE_ALG_RSA,
//         server_proprietary_certificate::dw_key_alg_id = KEY_EXCHANGE_ALG_RSA,
//         server_proprietary_certificate::w_public_key_blob_type = BB_RSA_KEY_BLOB,
//         server_proprietary_certificate::w_public_key_blob_len = 92_c,
//         server_proprietary_certificate::rsapk::magic = 0x31415352_c /* TODO RSA_MAGIC */,
//         server_proprietary_certificate::rsapk::key_len = 72_c,
//         server_proprietary_certificate::rsapk::bit_len = 512_c,
//         server_proprietary_certificate::rsapk::data_len = 63_c,
//         server_proprietary_certificate::rsapk::pub_exp,
//         server_proprietary_certificate::rsapk::modulus,
//         proto::clear_bytes<class pad1, SEC_PADDING_SIZE>{},
//         server_proprietary_certificate::w_signature_blob_type = BB_RSA_SIGNATURE_BLOB,
//         server_proprietary_certificate::w_signature_blob,
//         proto::clear_bytes<class pad2, SEC_PADDING_SIZE>{}
//     );
//
//     constexpr auto sc_security = proto::choose(
//         !encryption_method && !encryption_level,
//         sc_security_without_encryption,
//         sc_security_with_encryption
//     );
// } }

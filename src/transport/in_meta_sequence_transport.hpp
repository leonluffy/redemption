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
 *   Copyright (C) Wallix 2010-2013
 *   Author(s): Christophe Grosjean, Raphael Zhou, Jonathan Poelen, Meng Tan
 */

#ifndef REDEMPTION_TRANSPORT_IN_META_SEQUENCE_TRANSPORT_HPP
#define REDEMPTION_TRANSPORT_IN_META_SEQUENCE_TRANSPORT_HPP

#include "transport/detail/meta_opener.hpp"
#include "transport/mixin_transport.hpp"
#include "transport/buffer/file_buf.hpp"
#include "transport/filter/crypto_filter.hpp"
#include "urandom_read.hpp"

namespace transbuf {
    class icrypto_filename_buf
    {
        transfil::decrypt_filter decrypt;
        CryptoContext * cctx;
        ifile_buf file;

    public:
        explicit icrypto_filename_buf(CryptoContext * cctx)
        : cctx(cctx)
        {}

        int open(const char * filename, mode_t mode = 0600)
        {
            unsigned char trace_key[CRYPTO_KEY_LENGTH]; // derived key for cipher
            unsigned char derivator[DERIVATOR_LENGTH];

            this->cctx->get_derivator(filename, derivator, DERIVATOR_LENGTH);
            if (-1 == this->cctx->compute_hmac(trace_key, derivator)) {
                return -1;
            }

            int err = this->file.open(filename, mode);
            if (err < 0) {
                return err;
            }

            return this->decrypt.open(this->file, trace_key);
        }

        ssize_t read(void * data, size_t len)
        { return this->decrypt.read(this->file, data, len); }

        int close()
        { return this->file.close(); }

        bool is_open() const noexcept
        { return this->file.is_open(); }

        off64_t seek(off64_t offset, int whence) const
        { return this->file.seek(offset, whence); }
    };
}

struct InMetaSequenceTransport
: InputNextTransport<detail::in_meta_sequence_buf<
    detail::empty_ctor</*transbuf::ibuffering_buf<*/transbuf::ifile_buf/*> */>,
    detail::empty_ctor<transbuf::ifile_buf>
> >
{
    InMetaSequenceTransport(CryptoContext * cctx, const char * filename, const char * extension, uint32_t verbose = 0)
    : InMetaSequenceTransport::TransportType(
        detail::in_meta_sequence_buf_param<>(detail::temporary_concat(filename, extension).str, verbose))
    {
        this->verbose = verbose;
    }

    explicit InMetaSequenceTransport(CryptoContext * cctx, const char * filename, uint32_t verbose = 0)
    : InMetaSequenceTransport::TransportType(detail::in_meta_sequence_buf_param<>(filename, verbose))
    {
        this->verbose = verbose;
    }

    unsigned begin_chunk_time() const noexcept
    { return this->buffer().get_begin_chunk_time(); }

    unsigned end_chunk_time() const noexcept
    { return this->buffer().get_end_chunk_time(); }

    const char * path() const noexcept
    { return this->buffer().current_path(); }
};

struct CryptoInMetaSequenceTransport
: InputNextTransport<detail::in_meta_sequence_buf<
    transbuf::icrypto_filename_buf,
    transbuf::icrypto_filename_buf
> >
{
    CryptoInMetaSequenceTransport(CryptoContext * crypto_ctx, const char * filename, const char * extension)
    : CryptoInMetaSequenceTransport::TransportType(
        detail::in_meta_sequence_buf_param<CryptoContext*,CryptoContext*>(
            detail::temporary_concat(filename, extension).c_str(), 0, crypto_ctx, crypto_ctx))
    {}

    CryptoInMetaSequenceTransport(CryptoContext * crypto_ctx, const char * filename)
    : CryptoInMetaSequenceTransport::TransportType(
        detail::in_meta_sequence_buf_param<CryptoContext*,CryptoContext*>(filename, 0, crypto_ctx, crypto_ctx))
    {}

    time_t begin_chunk_time() const noexcept
    { return this->buffer().get_begin_chunk_time(); }

    time_t end_chunk_time() const noexcept
    { return this->buffer().get_end_chunk_time(); }

    const char * path() const noexcept
    { return this->buffer().current_path(); }
};

#endif

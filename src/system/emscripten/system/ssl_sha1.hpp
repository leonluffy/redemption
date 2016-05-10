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
   Copyright (C) Wallix 2010-2014
   Author(s): Christophe Grosjean, Javier Caverni, Meng Tan

   openssl headers

   Based on xrdp and rdesktop
   Copyright (C) Jay Sorg 2004-2010
   Copyright (C) Matthew Chapman 1999-2007
*/

#pragma once

#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <cassert>

#include "core/error.hpp"
#include "utils/log.hpp"
#include "utils/bitfu.hpp"






class SslSha1_direct
{
    struct sha1 {
        uint32_t state[5];
        uint32_t count[2];
        unsigned char buffer[64];
    } sha1;


    uint32_t rol(uint32_t value, int bits) {
        return ( (value << bits) | (value >> (32 - bits)) );
    }

    uint32_t PUT_UINT32(int j, uint32_t value, unsigned char * block) {
        uint8_t a = (value >> 24) & 0xFF;
        uint8_t b = (value >> 16) & 0xFF;
        uint8_t c = (value >> 8) & 0xFF;
        uint8_t d = value & 0xFF;
        block[j+0] = d;
        block[j+1] = c;
        block[j+2] = b;
        block[j+3] = a;
        return value;
    }

    uint32_t blk0(int j, unsigned char * block) {
        return  (block[j+0] << 24)
            + (block[j+1] << 16)
            + (block[j+2] << 8)
            + block[j+3];
    }

    uint32_t blk1(int j, unsigned char * block) {
        return  (block[j+3] << 24)
            + (block[j+2] << 16)
            + (block[j+1] << 8)
            + block[j+0];
    }

    uint32_t block_xor(unsigned i, unsigned char * block)
    {
        return blk1(((i+13)&15)*4, block)
            ^ blk1(((i+8)&15)*4, block)
            ^ blk1(((i+2)&15)*4, block)
            ^ blk1(((i+0)&15)*4, block);
    }

    /* (R0+R1), R2, R3, R4 are the different operations used in SHA1 */
    void R0(uint32_t v, uint32_t & w, uint32_t x, uint32_t y, uint32_t & z, int i, unsigned char * block) {
        z += ((w&(x^y))^y) + PUT_UINT32(i*4, blk0(i*4, block), block) + 0x5A827999 + ( (v << 5) | (v >> 27) );
        w = (w << 30) | (w >> 2);
    }

    void R1(uint32_t v, uint32_t & w, uint32_t x, uint32_t y, uint32_t & z, int i, unsigned char * block) {
        z+=((w&(x^y))^y)+PUT_UINT32((i&15)*4, rol(block_xor(i, block),1), block)+0x5A827999 + ( (v << 5) | (v >> 27) );
        w = (w << 30) | (w >> 2);
    }

    void R2(uint32_t v, uint32_t & w, uint32_t x, uint32_t y, uint32_t & z, int i, unsigned char * block) {
        z+=(w^x^y)+PUT_UINT32((i&15)*4, rol(block_xor(i, block),1), block)+0x6ED9EBA1 + ( (v << 5) | (v >> 27) );
        w = (w << 30) | (w >> 2);
    }

    void R3(uint32_t v, uint32_t & w, uint32_t x, uint32_t y, uint32_t & z, int i, unsigned char * block) {
        z+=(((w|x)&y)|(w&x))+PUT_UINT32((i&15)*4, rol(block_xor(i, block),1), block)+0x8F1BBCDC + ( (v << 5) | (v >> 27) );
        w = (w << 30) | (w >> 2);
    }

    void R4(uint32_t v, uint32_t & w, uint32_t x, uint32_t y, uint32_t & z, int i, unsigned char * block) {
        z+=(w^x^y)+PUT_UINT32((i&15)*4, rol(block_xor(i, block),1), block)+0xCA62C1D6 + ( (v << 5) | (v >> 27) );
        w = (w << 30) | (w >> 2);
    }


    /* Hash a single 512-bit block. This is the core of the algorithm. */
    void SHA1Transform(uint32_t state[5], const unsigned char buffer[64])
    {
        uint32_t a, b, c, d, e;

        unsigned char block[64];

        memcpy(block, buffer, 64);


        /* Copy context->state[] to working vars */
        a = state[0];
        b = state[1];
        c = state[2];
        d = state[3];
        e = state[4];
        /* 4 rounds of 20 operations each. Loop unrolled. */
        R0(a,b,c,d,e, 0, block); R0(e,a,b,c,d, 1, block); R0(d,e,a,b,c, 2, block); R0(c,d,e,a,b, 3, block);
        R0(b,c,d,e,a, 4, block); R0(a,b,c,d,e, 5, block); R0(e,a,b,c,d, 6, block); R0(d,e,a,b,c, 7, block);
        R0(c,d,e,a,b, 8, block); R0(b,c,d,e,a, 9, block); R0(a,b,c,d,e,10, block); R0(e,a,b,c,d,11, block);
        R0(d,e,a,b,c,12, block); R0(c,d,e,a,b,13, block); R0(b,c,d,e,a,14, block); R0(a,b,c,d,e,15, block);
        R1(e,a,b,c,d,16, block); R1(d,e,a,b,c,17, block); R1(c,d,e,a,b,18, block); R1(b,c,d,e,a,19, block);
        R2(a,b,c,d,e,20, block); R2(e,a,b,c,d,21, block); R2(d,e,a,b,c,22, block); R2(c,d,e,a,b,23, block);
        R2(b,c,d,e,a,24, block); R2(a,b,c,d,e,25, block); R2(e,a,b,c,d,26, block); R2(d,e,a,b,c,27, block);
        R2(c,d,e,a,b,28, block); R2(b,c,d,e,a,29, block); R2(a,b,c,d,e,30, block); R2(e,a,b,c,d,31, block);
        R2(d,e,a,b,c,32, block); R2(c,d,e,a,b,33, block); R2(b,c,d,e,a,34, block); R2(a,b,c,d,e,35, block);
        R2(e,a,b,c,d,36, block); R2(d,e,a,b,c,37, block); R2(c,d,e,a,b,38, block); R2(b,c,d,e,a,39, block);
        R3(a,b,c,d,e,40, block); R3(e,a,b,c,d,41, block); R3(d,e,a,b,c,42, block); R3(c,d,e,a,b,43, block);
        R3(b,c,d,e,a,44, block); R3(a,b,c,d,e,45, block); R3(e,a,b,c,d,46, block); R3(d,e,a,b,c,47, block);
        R3(c,d,e,a,b,48, block); R3(b,c,d,e,a,49, block); R3(a,b,c,d,e,50, block); R3(e,a,b,c,d,51, block);
        R3(d,e,a,b,c,52, block); R3(c,d,e,a,b,53, block); R3(b,c,d,e,a,54, block); R3(a,b,c,d,e,55, block);
        R3(e,a,b,c,d,56, block); R3(d,e,a,b,c,57, block); R3(c,d,e,a,b,58, block); R3(b,c,d,e,a,59, block);
        R4(a,b,c,d,e,60, block); R4(e,a,b,c,d,61, block); R4(d,e,a,b,c,62, block); R4(c,d,e,a,b,63, block);
        R4(b,c,d,e,a,64, block); R4(a,b,c,d,e,65, block); R4(e,a,b,c,d,66, block); R4(d,e,a,b,c,67, block);
        R4(c,d,e,a,b,68, block); R4(b,c,d,e,a,69, block); R4(a,b,c,d,e,70, block); R4(e,a,b,c,d,71, block);
        R4(d,e,a,b,c,72, block); R4(c,d,e,a,b,73, block); R4(b,c,d,e,a,74, block); R4(a,b,c,d,e,75, block);
        R4(e,a,b,c,d,76, block); R4(d,e,a,b,c,77, block); R4(c,d,e,a,b,78, block); R4(b,c,d,e,a,79, block);
        /* Add the working vars back into context.state[] */
        state[0] += a;
        state[1] += b;
        state[2] += c;
        state[3] += d;
        state[4] += e;
    }


    /* SHA1Init - Initialize new context */
    void SHA1Init(struct sha1* context)
    {
        /* SHA1 initialization constants */
        context->state[0] = 0x67452301;
        context->state[1] = 0xEFCDAB89;
        context->state[2] = 0x98BADCFE;
        context->state[3] = 0x10325476;
        context->state[4] = 0xC3D2E1F0;
        context->count[0] = context->count[1] = 0;
    }


    /* Run your data through this. */
    void SHA1Update(struct sha1* context, const unsigned char* data, uint32_t len)
    {
    uint32_t i;
    uint32_t j;

        j = context->count[0];
        if ((context->count[0] += len << 3) < j)
        context->count[1]++;
        context->count[1] += (len>>29);
        j = (j >> 3) & 63;
        if ((j + len) > 63) {
            memcpy(&context->buffer[j], data, (i = 64-j));
            SHA1Transform(context->state, context->buffer);
            for ( ; i + 63 < len; i += 64) {
                SHA1Transform(context->state, &data[i]);
            }
            j = 0;
        }
        else i = 0;
        memcpy(&context->buffer[j], &data[i], len - i);
    }


    /* Add padding and return the message digest. */
    void SHA1Final(unsigned char digest[20], struct sha1* context)
    {
    unsigned i;
    unsigned char finalcount[8];
    unsigned char c;


        for (i = 0; i < 8; i++) {
            finalcount[i] = static_cast<unsigned char>((context->count[(i >= 4 ? 0 : 1)]
             >> ((3-(i & 3)) * 8) ) & 255);  /* Endian independent */
        }

        c = 0200;
        SHA1Update(context, &c, 1);
        while ((context->count[0] & 504) != 448) {
        c = 0000;
            SHA1Update(context, &c, 1);
        }
        SHA1Update(context, finalcount, 8);  /* Should cause a SHA1Transform() */
        for (i = 0; i < 20; i++) {
            digest[i] = static_cast<unsigned char>
             ((context->state[i>>2] >> ((3-(i & 3)) * 8) ) & 255);
        }
        /* Wipe variables */
        memset(context, '\0', sizeof(*context));
        memset(&finalcount, '\0', sizeof(finalcount));
    }


    public:
    SslSha1_direct()
    {
        SslSha1_direct::SHA1Init(&this->sha1);
    }

    void update(const uint8_t * const data, size_t data_size)
    {
        SslSha1_direct::SHA1Update(&this->sha1, data, data_size);
    }

    void final(unsigned char digest[20], size_t data_size)
    {
        SslSha1_direct::SHA1Final(digest, &this->sha1);
    }
};


class SslHMAC_Sha1_direct
{
    uint8_t k_ipad[64];
    uint8_t k_opad[64];
    SslSha1_direct context;

    public:
    SslHMAC_Sha1_direct(const uint8_t * const key, size_t key_len)
        : k_ipad{
            0x36, 0x36, 0x36, 0x36, 0x36, 0x36, 0x36, 0x36,
            0x36, 0x36, 0x36, 0x36, 0x36, 0x36, 0x36, 0x36,
            0x36, 0x36, 0x36, 0x36, 0x36, 0x36, 0x36, 0x36,
            0x36, 0x36, 0x36, 0x36, 0x36, 0x36, 0x36, 0x36,
            0x36, 0x36, 0x36, 0x36, 0x36, 0x36, 0x36, 0x36,
            0x36, 0x36, 0x36, 0x36, 0x36, 0x36, 0x36, 0x36,
            0x36, 0x36, 0x36, 0x36, 0x36, 0x36, 0x36, 0x36,
            0x36, 0x36, 0x36, 0x36, 0x36, 0x36, 0x36, 0x36
         },
         k_opad{
            0x5C, 0x5C, 0x5C, 0x5C, 0x5C, 0x5C, 0x5C, 0x5C,
            0x5C, 0x5C, 0x5C, 0x5C, 0x5C, 0x5C, 0x5C, 0x5C,
            0x5C, 0x5C, 0x5C, 0x5C, 0x5C, 0x5C, 0x5C, 0x5C,
            0x5C, 0x5C, 0x5C, 0x5C, 0x5C, 0x5C, 0x5C, 0x5C,
            0x5C, 0x5C, 0x5C, 0x5C, 0x5C, 0x5C, 0x5C, 0x5C,
            0x5C, 0x5C, 0x5C, 0x5C, 0x5C, 0x5C, 0x5C, 0x5C,
            0x5C, 0x5C, 0x5C, 0x5C, 0x5C, 0x5C, 0x5C, 0x5C,
            0x5C, 0x5C, 0x5C, 0x5C, 0x5C, 0x5C, 0x5C, 0x5C,
         }
    {
         const uint8_t * k = key;
         if (key_len > 64) {
             unsigned char digest[SHA1_DIGEST_LENGTH];
             SslSha1_direct sha1;
             sha1.update(digest, SHA1_DIGEST_LENGTH);
             sha1.final(digest, SHA1_DIGEST_LENGTH);
             key_len = SHA1_DIGEST_LENGTH;
             k = key;
         }
         size_t i;
         for (i = 0; i < key_len; i++){
            k_ipad[i] ^= k[i];
            k_opad[i] ^= k[i];
         }
         context.update(k_ipad, 64);
    }

    ~SslHMAC_Sha1_direct()
    {
    }

    void update(const uint8_t * const data, size_t data_size)
    {
        context.update(data, data_size);
    }

    void final(uint8_t * out_data, size_t out_data_size)
    {
        assert(SHA1_DIGEST_LENGTH == out_data_size);
        context.final(out_data, SHA1_DIGEST_LENGTH);

        SslSha1_direct sha1;
        sha1.update(this->k_opad, 64);
        sha1.update(out_data, SHA1_DIGEST_LENGTH);
        sha1.final(out_data, SHA1_DIGEST_LENGTH);
    }
};

typedef SslHMAC_Sha1_direct SslHMAC_Sha1;
typedef SslSha1_direct SslSha1;


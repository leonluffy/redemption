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
   Copyright (C) Wallix 2010-2013
   Author(s): Christophe Grosjean
   Based on unit tests imported from FreeRDP (test_mppc*)
   from Laxmikant Rashinkar.

   Unit test for MPPC compression
*/

#define BOOST_AUTO_TEST_MAIN
#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MODULE TestMPPC
#include <boost/test/auto_unit_test.hpp>

#define LOGNULL
#include "log.hpp"

#include <stdint.h>
#include <sys/time.h>
#include "RDP/mppc.hpp"

BOOST_AUTO_TEST_CASE(TestMPPC)
{
    // Load compressed_rd5 and decompressed_rd5
    #include "../../fixtures/test_mppc_TestMPPC.hpp"

    const uint8_t * rdata;
    uint32_t        rlen;
    long int dur;

    struct timeval start_time;
    struct timeval end_time;


    /* save starting time */
    gettimeofday(&start_time, NULL);

    for (int x = 0; x < 1000 ; x++){
        struct rdp_mppc_dec* rmppc = new rdp_mppc_unified_dec();

        /* uncompress data */
        BOOST_CHECK_EQUAL(true, rmppc->decompress(compressed_rd5, sizeof(compressed_rd5), PACKET_COMPRESSED | PACKET_COMPR_TYPE_64K, rdata, rlen));

        BOOST_CHECK_EQUAL(0, memcmp(decompressed_rd5, rdata, sizeof(decompressed_rd5)));
        delete rmppc;
    }

    /* get end time */
    gettimeofday(&end_time, NULL);

    /* print time taken */
    dur = ((end_time.tv_sec - start_time.tv_sec) * 1000000) + (end_time.tv_usec - start_time.tv_usec);
    LOG(LOG_INFO, "test_mppc: decompressed data in %ld micro seconds", dur);
}

BOOST_AUTO_TEST_CASE(TestMPPC_enc)
{
    // Load decompressed_rd5_data
    #include "../../fixtures/test_mppc_TestMPPC_enc.hpp"

    const uint8_t * rdata;
    uint32_t        rlen;

    /* required for timing the test */
    struct timeval start_time;
    struct timeval end_time;

    /* setup decoder */
    struct rdp_mppc_dec * rmppc = new rdp_mppc_unified_dec();

    /* setup encoder for RDP 5.0 */
    struct rdp_mppc_50_enc * enc = new rdp_mppc_50_enc();

    int data_len = sizeof(decompressed_rd5_data);
    LOG(LOG_INFO, "test_mppc_enc: testing with embedded data of %d bytes", data_len);

    /* save starting time */
    gettimeofday(&start_time, NULL);

    uint8_t  compressionFlags;
    uint16_t datalen;

    BOOST_CHECK_EQUAL(true, enc->compress(decompressed_rd5_data, data_len, compressionFlags, datalen,
        rdp_mppc_enc::MAX_COMPRESSED_DATA_SIZE_UNUSED));

    BOOST_CHECK(0 != (compressionFlags & PACKET_COMPRESSED));
    BOOST_CHECK_EQUAL(true,
        rmppc->decompress((uint8_t*)enc->outputBuffer, enc->bytes_in_opb, enc->flags, rdata, rlen));
    BOOST_CHECK_EQUAL(data_len, rlen);
    BOOST_CHECK_EQUAL(0, memcmp(decompressed_rd5_data, rdata, rlen));

    /* get end time */
    gettimeofday(&end_time, NULL);

    /* print time taken */
    long int dur = ((end_time.tv_sec - start_time.tv_sec) * 1000000) + (end_time.tv_usec - start_time.tv_usec);
    LOG(LOG_INFO, "test_mppc_enc: compressed %d bytes in %f seconds\n", data_len, (float) (dur) / 1000000.0F);

    delete enc;
    delete rmppc;
}

BOOST_AUTO_TEST_CASE(TestBitsSerializer)
{
    uint8_t outputBuffer[256] ={};
    int bits_left = 8;
    int opb_index = 0;
    insert_n_bits_40_50(2, 3, outputBuffer, bits_left, opb_index);
    BOOST_CHECK_EQUAL(6, bits_left);
    BOOST_CHECK_EQUAL(0, opb_index);
    BOOST_CHECK_EQUAL(192, outputBuffer[0] & 0xFF);

    insert_n_bits_40_50(2, 3, outputBuffer, bits_left, opb_index);
    BOOST_CHECK_EQUAL(4, bits_left);
    BOOST_CHECK_EQUAL(0, opb_index);
    BOOST_CHECK_EQUAL(0xF0, outputBuffer[0] & 0xFF);

    insert_n_bits_40_50(2, 3, outputBuffer, bits_left, opb_index);
    BOOST_CHECK_EQUAL(2, bits_left);
    BOOST_CHECK_EQUAL(0, opb_index);
    BOOST_CHECK_EQUAL(0xFc, outputBuffer[0] & 0xFF);

    insert_n_bits_40_50(2, 3, outputBuffer, bits_left, opb_index);
    BOOST_CHECK_EQUAL(8, bits_left);
    BOOST_CHECK_EQUAL(1, opb_index);
    BOOST_CHECK_EQUAL(0xFF, outputBuffer[0] & 0xFF);
}

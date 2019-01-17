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
   Copyright (C) Wallix 2012
   Author(s): Christophe Grosjean, Dominique Lafages
   Based on xrdp Copyright (C) Jay Sorg 2004-2010

   header file. Keylayout object, used by keymap managers
*/


#pragma once

#include "keyboard/keylayout.hpp"

namespace x00000426 {    // Latvian (Latvia) // Latvian

const static int LCID = 0x00000426;

const static char * const locale_name = "lv-LV";

const Keylayout::KeyLayout_t noMod = {
    /* x00 - x07 */    0x0000, 0x001B,    '1',    '2',    '3',    '4',    '5',    '6',
    /* x08 - x0F */       '7',    '8',    '9',    '0', 0x002D,    'f', 0x0008, 0x0009,
    /* x10 - x17 */    0x016B,    'g',    'j',    'r',    'm',    'v',    'n',    'z',
    /* x18 - x1F */    0x0113, 0x010D, 0x017E,    'h', 0x000D, 0x0000, 0x0161,    'u',
    /* x20 - x27 */       's',    'i',    'l',    'd',    'a',    't',    'e',    'c',
    /* x28 - x2F */    0x00B4, 0x00AD, 0x0000, 0x0137, 0x0146,    'b', 0x012B,    'k',
    /* x30 - x37 */       'p',    'o', 0x0101, 0x002C, 0x002E, 0x013C, 0x0000,    '*',
    /* x38 - x3F */    0x0000, 0x0020, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x40 - x47 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,    '7',
    /* x48 - x4F */       '8',    '9',    '-',    '4',    '5',    '6',    '+',    '1',
    /* x50 - x57 */       '2',    '3',    '0', 0x002C, 0x0000, 0x0000, 0x0123, 0x0000,
    /* x58 - x5F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x60 - x67 */    0x0000, 0x0000,    '/', 0x0000, 0x000D, 0x0000, 0x0000, 0x0000,
    /* x68 - x6F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x70 - x77 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x78 - x7F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
};

const Keylayout::KeyLayout_t shift = {
    /* x00 - x07 */    0x0000, 0x001B, 0x0021, 0x00AB, 0x00BB, 0x0024, 0x0025, 0x002F,
    /* x08 - x0F */    0x0026, 0x00D7, 0x0028, 0x0029, 0x005F,    'F', 0x0008, 0x0000,
    /* x10 - x17 */    0x016A,    'G',    'J',    'R',    'M',    'V',    'N',    'Z',
    /* x18 - x1F */    0x0112, 0x010C, 0x017D,    'H', 0x000D, 0x0000, 0x0160,    'U',
    /* x20 - x27 */       'S',    'I',    'L',    'D',    'A',    'T',    'E',    'C',
    /* x28 - x2F */    0x00B0, 0x003F, 0x0000, 0x0136, 0x0145,    'B', 0x012A,    'K',
    /* x30 - x37 */       'P',    'O', 0x0100, 0x003B, 0x003A, 0x013B, 0x0000,    '*',
    /* x38 - x3F */    0x0000, 0x0020, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x40 - x47 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x48 - x4F */    0x0000, 0x0000,    '-', 0x0000, 0x0000, 0x0000,    '+', 0x0000,
    /* x50 - x57 */    0x0000, 0x0000, 0x0000, 0x002C, 0x0000, 0x0000, 0x0122, 0x0000,
    /* x58 - x5F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x60 - x67 */    0x0000, 0x0000, 0x0000, 0x007F, 0x000D, 0x0000, 0x0000, 0x0000,
    /* x68 - x6F */       '/', 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x70 - x77 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x78 - x7F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
};

const Keylayout::KeyLayout_t altGr = {
    /* x00 - x07 */    0x0000, 0x001B, 0x00AB, 0x0000, 0x0000, 0x20AC, 0x0022, 0x2019,
    /* x08 - x0F */    0x0000, 0x003A, 0x0000, 0x0000, 0x2013, 0x003D, 0x0008, 0x0009,
    /* x10 - x17 */       'q', 0x0123, 0x0000, 0x0157,    'w',    'y', 0x0000, 0x0000,
    /* x18 - x1F */    0x0000, 0x0000, 0x005B, 0x005D, 0x000D, 0x0000, 0x0000, 0x0000,
    /* x20 - x27 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x20AC, 0x0000,
    /* x28 - x2F */    0x00B4, 0x0000, 0x0000, 0x0000, 0x0000,    'x', 0x0000, 0x0137,
    /* x30 - x37 */    0x0000, 0x00F5, 0x0000, 0x003C, 0x003E, 0x0000, 0x0000,    '*',
    /* x38 - x3F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x40 - x47 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x48 - x4F */    0x0000, 0x0000,    '-', 0x0000, 0x0000, 0x0000,    '+', 0x0000,
    /* x50 - x57 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x005C, 0x0000,
    /* x58 - x5F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x60 - x67 */    0x0000, 0x0000,    '/', 0x0000, 0x000D, 0x0000, 0x0000, 0x0000,
    /* x68 - x6F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x70 - x77 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x78 - x7F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
};

const Keylayout::KeyLayout_t shiftAltGr = {
    /* x00 - x07 */    0x0000, 0x001B, 0x0000, 0x0040, 0x0023, 0x0024, 0x007E, 0x005E,
    /* x08 - x0F */    0x00B1, 0x0000, 0x0000, 0x0000, 0x2014, 0x003B, 0x0008, 0x0009,
    /* x10 - x17 */       'Q', 0x0122, 0x0000, 0x0156,    'W',    'Y', 0x0000, 0x0000,
    /* x18 - x1F */    0x0000, 0x0000, 0x007B, 0x007D, 0x000D, 0x0000, 0x0000, 0x0000,
    /* x20 - x27 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x28 - x2F */    0x00A8, 0x0000, 0x0000, 0x0000, 0x0000,    'X', 0x0000, 0x0136,
    /* x30 - x37 */    0x0000, 0x00D5, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,    '*',
    /* x38 - x3F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x40 - x47 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x48 - x4F */    0x0000, 0x0000,    '-', 0x0000, 0x0000, 0x0000,    '+', 0x0000,
    /* x50 - x57 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x007C, 0x0000,
    /* x58 - x5F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x60 - x67 */    0x0000, 0x0000,    '/', 0x0000, 0x000D, 0x0000, 0x0000, 0x0000,
    /* x68 - x6F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x70 - x77 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x78 - x7F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
};

const Keylayout::KeyLayout_t ctrl = {
    /* x00 - x07 */    0x0000, 0x001B, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x08 - x0F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0008, 0x0009,
    /* x10 - x17 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x18 - x1F */    0x0000, 0x0000, 0x0000, 0x0000, 0x000D, 0x0000, 0x0000, 0x0000,
    /* x20 - x27 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x28 - x2F */    0x0000, 0x0000, 0x0000, 0x001C, 0x001B, 0x0000, 0x001D, 0x0000,
    /* x30 - x37 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,    '*',
    /* x38 - x3F */    0x0000, 0x0020, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x40 - x47 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x48 - x4F */    0x0000, 0x0000,    '-', 0x0000, 0x0000, 0x0000,    '+', 0x0000,
    /* x50 - x57 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x001C, 0x0000,
    /* x58 - x5F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x60 - x67 */    0x0000, 0x0000,    '/', 0x0000, 0x000D, 0x0000, 0x0000, 0x0000,
    /* x68 - x6F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x70 - x77 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x78 - x7F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
};

const Keylayout::KeyLayout_t capslock_noMod = {
    /* x00 - x07 */    0x0000, 0x001B,    '1',    '2',    '3',    '4',    '5',    '6',
    /* x08 - x0F */       '7',    '8',    '9',    '0', 0x002D,    'F', 0x0008, 0x0009,
    /* x10 - x17 */    0x016A,    'G',    'J',    'R',    'M',    'V',    'N',    'Z',
    /* x18 - x1F */    0x0112, 0x010C, 0x017D,    'H', 0x000D, 0x0000, 0x0160,    'U',
    /* x20 - x27 */       'S',    'I',    'L',    'D',    'A',    'T',    'E',    'C',
    /* x28 - x2F */    0x00B4, 0x00AD, 0x0000, 0x0136, 0x0145,    'B', 0x012A,    'K',
    /* x30 - x37 */       'P',    'O', 0x0100, 0x002C, 0x002E, 0x013B, 0x0000,    '*',
    /* x38 - x3F */    0x0000, 0x0020, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x40 - x47 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x48 - x4F */    0x0000, 0x0000,    '-', 0x0000, 0x0000, 0x0000,    '+', 0x0000,
    /* x50 - x57 */    0x0000, 0x0000, 0x0000, 0x002C, 0x0000, 0x0000, 0x0122, 0x0000,
    /* x58 - x5F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x60 - x67 */    0x0000, 0x0000,    '/', 0x0000, 0x000D, 0x0000, 0x0000, 0x0000,
    /* x68 - x6F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x70 - x77 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x78 - x7F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
};

const Keylayout::KeyLayout_t capslock_shift = {
    /* x00 - x07 */    0x0000, 0x001B, 0x0021, 0x00AB, 0x00BB, 0x0024, 0x0025, 0x002F,
    /* x08 - x0F */    0x0026, 0x00D7, 0x0028, 0x0029, 0x005F,    'f', 0x0008, 0x0009,
    /* x10 - x17 */    0x016B,    'g',    'j',    'r',    'm',    'v',    'n',    'z',
    /* x18 - x1F */    0x0113, 0x010D, 0x017E,    'h', 0x000D, 0x0000, 0x0161,    'u',
    /* x20 - x27 */       's',    'i',    'l',    'd',    'a',    't',    'e',    'c',
    /* x28 - x2F */    0x00B0, 0x003F, 0x0000, 0x0137, 0x0146,    'b', 0x012B,    'k',
    /* x30 - x37 */       'p',    'o', 0x0101, 0x003B, 0x003A, 0x013C, 0x0000,    '*',
    /* x38 - x3F */    0x0000, 0x0020, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x40 - x47 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x48 - x4F */    0x0000, 0x0000,    '-', 0x0000, 0x0000, 0x0000,    '+', 0x0000,
    /* x50 - x57 */    0x0000, 0x0000, 0x0000, 0x002C, 0x0000, 0x0000, 0x0123, 0x0000,
    /* x58 - x5F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x60 - x67 */    0x0000, 0x0000,    '/', 0x0000, 0x000D, 0x0000, 0x0000, 0x0000,
    /* x68 - x6F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x70 - x77 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x78 - x7F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
};

const Keylayout::KeyLayout_t capslock_altGr = {
    /* x00 - x07 */    0x0000, 0x001B, 0x00AB, 0x0000, 0x0000, 0x20AC, 0x0022, 0x2019,
    /* x08 - x0F */    0x0000, 0x003A, 0x0000, 0x0000, 0x2013, 0x003D, 0x0008, 0x0009,
    /* x10 - x17 */       'q', 0x0123, 0x0000, 0x0157,    'w',    'y', 0x0000, 0x0000,
    /* x18 - x1F */    0x0000, 0x0000, 0x005B, 0x005D, 0x000D, 0x0000, 0x0000, 0x0000,
    /* x20 - x27 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x20AC, 0x0000,
    /* x28 - x2F */    0x00B4, 0x0000, 0x0000, 0x0000, 0x0000,    'x', 0x0000, 0x0137,
    /* x30 - x37 */    0x0000, 0x00F5, 0x0000, 0x003C, 0x003E, 0x0000, 0x0000,    '*',
    /* x38 - x3F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x40 - x47 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x48 - x4F */    0x0000, 0x0000,    '-', 0x0000, 0x0000, 0x0000,    '+', 0x0000,
    /* x50 - x57 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x005C, 0x0000,
    /* x58 - x5F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x60 - x67 */    0x0000, 0x0000,    '/', 0x0000, 0x000D, 0x0000, 0x0000, 0x0000,
    /* x68 - x6F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x70 - x77 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x78 - x7F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
};

const Keylayout::KeyLayout_t capslock_shiftAltGr = {
    /* x00 - x07 */    0x0000, 0x001B, 0x0000, 0x0040, 0x0023, 0x0024, 0x007E, 0x005E,
    /* x08 - x0F */    0x00B1, 0x0000, 0x0000, 0x0000, 0x2014, 0x003B, 0x0008, 0x0009,
    /* x10 - x17 */       'Q', 0x0122, 0x0000, 0x0156,    'W',    'Y', 0x0000, 0x0000,
    /* x18 - x1F */    0x0000, 0x0000, 0x007B, 0x007D, 0x000D, 0x0000, 0x0000, 0x0000,
    /* x20 - x27 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x28 - x2F */    0x00A8, 0x0000, 0x0000, 0x0000, 0x0000,    'X', 0x0000, 0x0136,
    /* x30 - x37 */    0x0000, 0x00D5, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,    '*',
    /* x38 - x3F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x40 - x47 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x48 - x4F */    0x0000, 0x0000,    '-', 0x0000, 0x0000, 0x0000,    '+', 0x0000,
    /* x50 - x57 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x007C, 0x0000,
    /* x58 - x5F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x60 - x67 */    0x0000, 0x0000,    '/', 0x0000, 0x000D, 0x0000, 0x0000, 0x0000,
    /* x68 - x6F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x70 - x77 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x78 - x7F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
};

const Keylayout::dkey_t deadkeys[] = {
    { 0x007e, 0x06,  3, { {0x006f, 0x00f5}  // 'o' = 'õ'
                        , {0x0020, 0x007e}  // ' ' = '~'
                        , {0x004f, 0x00d5}  // 'O' = 'Õ'
                        }
    },
    { 0x00b4, 0x28, 13, { {0x006f, 0x00f3}  // 'o' = 'ó'
                        , {0x007a, 0x017a}  // 'z' = 'ź'
                        , {0x0020, 0x00b4}  // ' ' = '´'
                        , {0x006e, 0x0144}  // 'n' = 'ń'
                        , {0x0053, 0x015a}  // 'S' = 'Ś'
                        , {0x0043, 0x0106}  // 'C' = 'Ć'
                        , {0x0045, 0x00c9}  // 'E' = 'É'
                        , {0x0073, 0x015b}  // 's' = 'ś'
                        , {0x0065, 0x00e9}  // 'e' = 'é'
                        , {0x0063, 0x0107}  // 'c' = 'ć'
                        , {0x005a, 0x0179}  // 'Z' = 'Ź'
                        , {0x004e, 0x0143}  // 'N' = 'Ń'
                        , {0x004f, 0x00d3}  // 'O' = 'Ó'
                        }
    },
    { 0x00b0, 0x28,  8, { {0x007a, 0x017c}  // 'z' = 'ż'
                        , {0x0020, 0x00b0}  // ' ' = '°'
                        , {0x0041, 0x00c5}  // 'A' = 'Å'
                        , {0x0045, 0x0116}  // 'E' = 'Ė'
                        , {0x0067, 0x0121}  // 'g' = 'ġ'
                        , {0x0065, 0x0117}  // 'e' = 'ė'
                        , {0x0061, 0x00e5}  // 'a' = 'å'
                        , {0x005a, 0x017b}  // 'Z' = 'Ż'
                        }
    },
    { 0x00a8, 0x28,  7, { {0x006f, 0x00f6}  // 'o' = 'ö'
                        , {0x0020, 0x00a8}  // ' ' = '¨'
                        , {0x0041, 0x00c4}  // 'A' = 'Ä'
                        , {0x0055, 0x00dc}  // 'U' = 'Ü'
                        , {0x0075, 0x00fc}  // 'u' = 'ü'
                        , {0x0061, 0x00e4}  // 'a' = 'ä'
                        , {0x004f, 0x00d6}  // 'O' = 'Ö'
                        }
    },
};

const static uint8_t nbDeadkeys = 4;

} // END NAMESPACE - x00000426

static const Keylayout keylayout_x00000426( x00000426::LCID
                                          , x00000426::locale_name
                                          , x00000426::noMod
                                          , x00000426::shift
                                          , x00000426::altGr
                                          , x00000426::shiftAltGr
                                          , x00000426::ctrl
                                          , x00000426::capslock_noMod
                                          , x00000426::capslock_shift
                                          , x00000426::capslock_altGr
                                          , x00000426::capslock_shiftAltGr
                                          , x00000426::deadkeys
                                          , x00000426::nbDeadkeys
);


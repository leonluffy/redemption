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

#if !defined(__KEYLAYOUT_X00010405_HPP__)
#define __KEYLAYOUT_X00010405_HPP__

#include "keylayout.hpp"

namespace x00010405 {    // Czech (Czech Republic) // Czech (QWERTY)

const static int LCID = 0x00010405;

const Keylayout::KeyLayout_t noMod = {
    /* x00 - x07 */    0x0000, 0x001B, 0x002B, 0x011B, 0x0161, 0x010D, 0x0159, 0x017E,
    /* x08 - x0F */    0x00FD, 0x00E1, 0x00ED, 0x00E9, 0x003D, 0x00B4, 0x0008, 0x0009,
    /* x10 - x17 */       'q',    'w',    'e',    'r',    't',    'y',    'u',    'i',
    /* x18 - x1F */       'o',    'p', 0x00FA, 0x0029, 0x000D, 0x0000,    'a',    's',
    /* x20 - x27 */       'd',    'f',    'g',    'h',    'j',    'k',    'l', 0x016F,
    /* x28 - x2F */    0x00A7, 0x003B, 0x0000, 0x00A8,    'z',    'x',    'c',    'v',
    /* x30 - x37 */       'b',    'n',    'm', 0x002C, 0x002E, 0x002D, 0x0000,    '*',
    /* x38 - x3F */    0x0000, 0x0020, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x40 - x47 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,    '7',
    /* x48 - x4F */       '8',    '9',    '-',    '4',    '5',    '6',    '+',    '1',
    /* x50 - x57 */       '2',    '3',    '0', 0x002C, 0x0000, 0x0000, 0x005C, 0x0000,
    /* x58 - x5F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x60 - x67 */    0x0000, 0x0000,    '/', 0x0000, 0x000D, 0x0000, 0x0000, 0x0000,
    /* x68 - x6F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x70 - x77 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x78 - x7F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
};

const Keylayout::KeyLayout_t shift = {
    /* x00 - x07 */    0x0000, 0x001B,    '1',    '2',    '3',    '4',    '5',    '6',
    /* x08 - x0F */       '7',    '8',    '9',    '0', 0x0025, 0x02C7, 0x0008, 0x0000,
    /* x10 - x17 */       'Q',    'W',    'E',    'R',    'T',    'Y',    'U',    'I',
    /* x18 - x1F */       'O',    'P', 0x002F, 0x0028, 0x000D, 0x0000,    'A',    'S',
    /* x20 - x27 */       'D',    'F',    'G',    'H',    'J',    'K',    'L', 0x0022,
    /* x28 - x2F */    0x0021, 0x00B0, 0x0000, 0x0027,    'Z',    'X',    'C',    'V',
    /* x30 - x37 */       'B',    'N',    'M', 0x003F, 0x003A, 0x005F, 0x0000,    '*',
    /* x38 - x3F */    0x0000, 0x0020, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x40 - x47 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x48 - x4F */    0x0000, 0x0000,    '-', 0x0000, 0x0000, 0x0000,    '+', 0x0000,
    /* x50 - x57 */    0x0000, 0x0000, 0x0000, 0x002C, 0x0000, 0x0000, 0x007C, 0x0000,
    /* x58 - x5F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x60 - x67 */    0x0000, 0x0000, 0x0000, 0x007F, 0x000D, 0x0000, 0x0000, 0x0000,
    /* x68 - x6F */       '/', 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x70 - x77 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x78 - x7F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
};

const Keylayout::KeyLayout_t altGr = {
    /* x00 - x07 */    0x0000, 0x001B, 0x0021, 0x0040, 0x0023, 0x0024, 0x0025, 0x005E,
    /* x08 - x0F */    0x0026, 0x002A, 0x0028, 0x0029, 0x002D, 0x003D, 0x0008, 0x0009,
    /* x10 - x17 */    0x0000, 0x0000, 0x20AC, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x18 - x1F */    0x0000, 0x0000, 0x005B, 0x005D, 0x000D, 0x0000, 0x0000, 0x0000,
    /* x20 - x27 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x003B,
    /* x28 - x2F */    0x00A4, 0x0060, 0x0000, 0x005C, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x30 - x37 */    0x0000, 0x0000, 0x0000, 0x003C, 0x003E, 0x002F, 0x0000,    '*',
    /* x38 - x3F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x40 - x47 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x48 - x4F */    0x0000, 0x0000,    '-', 0x0000, 0x0000, 0x0000,    '+', 0x0000,
    /* x50 - x57 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x00DF, 0x0000,
    /* x58 - x5F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x60 - x67 */    0x0000, 0x0000,    '/', 0x0000, 0x000D, 0x0000, 0x0000, 0x0000,
    /* x68 - x6F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x70 - x77 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x78 - x7F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
};

const Keylayout::KeyLayout_t shiftAltGr = {
    /* x00 - x07 */    0x0000, 0x001B, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x08 - x0F */    0x0000, 0x0000, 0x0000, 0x0000, 0x005F, 0x002B, 0x0008, 0x0009,
    /* x10 - x17 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x18 - x1F */    0x0000, 0x0000, 0x007B, 0x007D, 0x000D, 0x0000, 0x0000, 0x0000,
    /* x20 - x27 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x003A,
    /* x28 - x2F */    0x005E, 0x007E, 0x0000, 0x007C, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x30 - x37 */    0x0000, 0x0000, 0x0000, 0x00D7, 0x00F7, 0x003F, 0x0000,    '*',
    /* x38 - x3F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x40 - x47 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x48 - x4F */    0x0000, 0x0000,    '-', 0x0000, 0x0000, 0x0000,    '+', 0x0000,
    /* x50 - x57 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x02DD, 0x0000,
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
    /* x18 - x1F */    0x0000, 0x0000, 0x0000, 0x001B, 0x000D, 0x0000, 0x0000, 0x0000,
    /* x20 - x27 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x001D,
    /* x28 - x2F */    0x0000, 0x0000, 0x0000, 0x001C, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x30 - x37 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,    '*',
    /* x38 - x3F */    0x0000, 0x0020, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x40 - x47 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x48 - x4F */    0x0000, 0x0000,    '-', 0x0000, 0x0000, 0x0000,    '+', 0x0000,
    /* x50 - x57 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x58 - x5F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x60 - x67 */    0x0000, 0x0000,    '/', 0x0000, 0x000D, 0x0000, 0x0000, 0x0000,
    /* x68 - x6F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x70 - x77 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x78 - x7F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
};

const Keylayout::KeyLayout_t capslock_noMod = {
    /* x00 - x07 */    0x0000, 0x001B, 0x002B, 0x011A, 0x0160, 0x010C, 0x0158, 0x017D,
    /* x08 - x0F */    0x00DD, 0x00C1, 0x00CD, 0x00C9, 0x003D, 0x00B4, 0x0008, 0x0009,
    /* x10 - x17 */       'Q',    'W',    'E',    'R',    'T',    'Y',    'U',    'I',
    /* x18 - x1F */       'O',    'P', 0x00DA, 0x0029, 0x000D, 0x0000,    'A',    'S',
    /* x20 - x27 */       'D',    'F',    'G',    'H',    'J',    'K',    'L', 0x016E,
    /* x28 - x2F */    0x00A7, 0x003B, 0x0000, 0x00A8,    'Z',    'X',    'C',    'V',
    /* x30 - x37 */       'B',    'N',    'M', 0x002C, 0x002E, 0x002D, 0x0000,    '*',
    /* x38 - x3F */    0x0000, 0x0020, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x40 - x47 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x48 - x4F */    0x0000, 0x0000,    '-', 0x0000, 0x0000, 0x0000,    '+', 0x0000,
    /* x50 - x57 */    0x0000, 0x0000, 0x0000, 0x002C, 0x0000, 0x0000, 0x005C, 0x0000,
    /* x58 - x5F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x60 - x67 */    0x0000, 0x0000,    '/', 0x0000, 0x000D, 0x0000, 0x0000, 0x0000,
    /* x68 - x6F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x70 - x77 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x78 - x7F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
};

const Keylayout::KeyLayout_t capslock_shift = {
    /* x00 - x07 */    0x0000, 0x001B,    '1', 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x08 - x0F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0025, 0x02C7, 0x0008, 0x0009,
    /* x10 - x17 */       'q',    'w',    'e',    'r',    't',    'y',    'u',    'i',
    /* x18 - x1F */       'o',    'p', 0x0000, 0x0028, 0x000D, 0x0000,    'a',    's',
    /* x20 - x27 */       'd',    'f',    'g',    'h',    'j',    'k',    'l', 0x0000,
    /* x28 - x2F */    0x0021, 0x00B0, 0x0000, 0x0027,    'z',    'x',    'c',    'v',
    /* x30 - x37 */       'b',    'n',    'm', 0x003F, 0x003A, 0x005F, 0x0000,    '*',
    /* x38 - x3F */    0x0000, 0x0020, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x40 - x47 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x48 - x4F */    0x0000, 0x0000,    '-', 0x0000, 0x0000, 0x0000,    '+', 0x0000,
    /* x50 - x57 */    0x0000, 0x0000, 0x0000, 0x002C, 0x0000, 0x0000, 0x007C, 0x0000,
    /* x58 - x5F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x60 - x67 */    0x0000, 0x0000,    '/', 0x0000, 0x000D, 0x0000, 0x0000, 0x0000,
    /* x68 - x6F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x70 - x77 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x78 - x7F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
};

const Keylayout::KeyLayout_t capslock_altGr = {
    /* x00 - x07 */    0x0000, 0x001B, 0x0021, 0x0040, 0x0023, 0x0024, 0x0025, 0x005E,
    /* x08 - x0F */    0x0026, 0x002A, 0x0028, 0x0029, 0x002D, 0x003D, 0x0008, 0x0009,
    /* x10 - x17 */    0x0000, 0x0000, 0x20AC, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x18 - x1F */    0x0000, 0x0000, 0x005B, 0x005D, 0x000D, 0x0000, 0x0000, 0x0000,
    /* x20 - x27 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x003B,
    /* x28 - x2F */    0x00A4, 0x0060, 0x0000, 0x005C, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x30 - x37 */    0x0000, 0x0000, 0x0000, 0x003C, 0x003E, 0x002F, 0x0000,    '*',
    /* x38 - x3F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x40 - x47 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x48 - x4F */    0x0000, 0x0000,    '-', 0x0000, 0x0000, 0x0000,    '+', 0x0000,
    /* x50 - x57 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x00DF, 0x0000,
    /* x58 - x5F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x60 - x67 */    0x0000, 0x0000,    '/', 0x0000, 0x000D, 0x0000, 0x0000, 0x0000,
    /* x68 - x6F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x70 - x77 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x78 - x7F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
};

const Keylayout::KeyLayout_t capslock_shiftAltGr = {
    /* x00 - x07 */    0x0000, 0x001B, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x08 - x0F */    0x0000, 0x0000, 0x0000, 0x0000, 0x005F, 0x002B, 0x0008, 0x0009,
    /* x10 - x17 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x18 - x1F */    0x0000, 0x0000, 0x007B, 0x007D, 0x000D, 0x0000, 0x0000, 0x0000,
    /* x20 - x27 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x003A,
    /* x28 - x2F */    0x005E, 0x007E, 0x0000, 0x007C, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x30 - x37 */    0x0000, 0x0000, 0x0000, 0x00D7, 0x00F7, 0x003F, 0x0000,    '*',
    /* x38 - x3F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x40 - x47 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x48 - x4F */    0x0000, 0x0000,    '-', 0x0000, 0x0000, 0x0000,    '+', 0x0000,
    /* x50 - x57 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x02DD, 0x0000,
    /* x58 - x5F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x60 - x67 */    0x0000, 0x0000,    '/', 0x0000, 0x000D, 0x0000, 0x0000, 0x0000,
    /* x68 - x6F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x70 - x77 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x78 - x7F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
};

const Keylayout::dkey_t deadkeys[] = {
    { 0x00b4, 0x0d, 25, { {0x0059, 0x00dd}  // 'Y' = 'Ý'
                        , {0x0053, 0x015a}  // 'S' = 'Ś'
                        , {0x0052, 0x0154}  // 'R' = 'Ŕ'
                        , {0x0055, 0x00da}  // 'U' = 'Ú'
                        , {0x004c, 0x0139}  // 'L' = 'Ĺ'
                        , {0x004e, 0x0143}  // 'N' = 'Ń'
                        , {0x004f, 0x00d3}  // 'O' = 'Ó'
                        , {0x007a, 0x017a}  // 'z' = 'ź'
                        , {0x0065, 0x00e9}  // 'e' = 'é'
                        , {0x0063, 0x0107}  // 'c' = 'ć'
                        , {0x0061, 0x00e1}  // 'a' = 'á'
                        , {0x0069, 0x00ed}  // 'i' = 'í'
                        , {0x0049, 0x00cd}  // 'I' = 'Í'
                        , {0x0020, 0x00b4}  // ' ' = '´'
                        , {0x0041, 0x00c1}  // 'A' = 'Á'
                        , {0x0043, 0x0106}  // 'C' = 'Ć'
                        , {0x0045, 0x00c9}  // 'E' = 'É'
                        , {0x005a, 0x0179}  // 'Z' = 'Ź'
                        , {0x006f, 0x00f3}  // 'o' = 'ó'
                        , {0x006e, 0x0144}  // 'n' = 'ń'
                        , {0x006c, 0x013a}  // 'l' = 'ĺ'
                        , {0x0073, 0x015b}  // 's' = 'ś'
                        , {0x0072, 0x0155}  // 'r' = 'ŕ'
                        , {0x0075, 0x00fa}  // 'u' = 'ú'
                        , {0x0079, 0x00fd}  // 'y' = 'ý'
                        }
    },
    { 0x02c7, 0x0d, 19, { {0x007a, 0x017e}  // 'z' = 'ž'
                        , {0x0043, 0x010c}  // 'C' = 'Č'
                        , {0x006e, 0x0148}  // 'n' = 'ň'
                        , {0x006c, 0x013e}  // 'l' = 'ľ'
                        , {0x0053, 0x0160}  // 'S' = 'Š'
                        , {0x0052, 0x0158}  // 'R' = 'Ř'
                        , {0x0054, 0x0164}  // 'T' = 'Ť'
                        , {0x0072, 0x0159}  // 'r' = 'ř'
                        , {0x0044, 0x010e}  // 'D' = 'Ď'
                        , {0x0073, 0x0161}  // 's' = 'š'
                        , {0x0045, 0x011a}  // 'E' = 'Ě'
                        , {0x0020, 0x02c7}  // ' ' = 'ˇ'
                        , {0x0064, 0x010f}  // 'd' = 'ď'
                        , {0x0065, 0x011b}  // 'e' = 'ě'
                        , {0x0063, 0x010d}  // 'c' = 'č'
                        , {0x005a, 0x017d}  // 'Z' = 'Ž'
                        , {0x004c, 0x013d}  // 'L' = 'Ľ'
                        , {0x004e, 0x0147}  // 'N' = 'Ň'
                        , {0x0074, 0x0165}  // 't' = 'ť'
                        }
    },
    { 0x005e, 0x28, 11, { {0x006f, 0x00f4}  // 'o' = 'ô'
                        , {0x0049, 0x00ce}  // 'I' = 'Î'
                        , {0x0020, 0x005e}  // ' ' = '^'
                        , {0x0041, 0x00c2}  // 'A' = 'Â'
                        , {0x0045, 0x00ca}  // 'E' = 'Ê'
                        , {0x0055, 0x00db}  // 'U' = 'Û'
                        , {0x0065, 0x00ea}  // 'e' = 'ê'
                        , {0x0075, 0x00fb}  // 'u' = 'û'
                        , {0x0061, 0x00e2}  // 'a' = 'â'
                        , {0x004f, 0x00d4}  // 'O' = 'Ô'
                        , {0x0069, 0x00ee}  // 'i' = 'î'
                        }
    },
    { 0x00b0, 0x29,  5, { {0x0041, 0x00c5}  // 'A' = 'Å'
                        , {0x0020, 0x00b0}  // ' ' = '°'
                        , {0x0075, 0x016f}  // 'u' = 'ů'
                        , {0x0055, 0x016e}  // 'U' = 'Ů'
                        , {0x0061, 0x00e5}  // 'a' = 'å'
                        }
    },
    { 0x00a8, 0x2b, 13, { {0x006f, 0x00f6}  // 'o' = 'ö'
                        , {0x0049, 0x00cf}  // 'I' = 'Ï'
                        , {0x0059, 0x0178}  // 'Y' = 'Ÿ'
                        , {0x0041, 0x00c4}  // 'A' = 'Ä'
                        , {0x0045, 0x00cb}  // 'E' = 'Ë'
                        , {0x0055, 0x00dc}  // 'U' = 'Ü'
                        , {0x0020, 0x00a8}  // ' ' = '¨'
                        , {0x0065, 0x00eb}  // 'e' = 'ë'
                        , {0x0075, 0x00fc}  // 'u' = 'ü'
                        , {0x0061, 0x00e4}  // 'a' = 'ä'
                        , {0x0079, 0x00ff}  // 'y' = 'ÿ'
                        , {0x004f, 0x00d6}  // 'O' = 'Ö'
                        , {0x0069, 0x00ef}  // 'i' = 'ï'
                        }
    },
    { 0x02dd, 0x56,  5, { {0x006f, 0x0151}  // 'o' = 'ő'
                        , {0x0020, 0x02dd}  // ' ' = '˝'
                        , {0x0075, 0x0171}  // 'u' = 'ű'
                        , {0x004f, 0x0150}  // 'O' = 'Ő'
                        , {0x0055, 0x0170}  // 'U' = 'Ű'
                        }
    },
};

const static uint8_t nbDeadkeys = 6;

} // END NAMESPACE - x00010405

static const Keylayout keylayout_x00010405( x00010405::LCID
                                          , x00010405::noMod
                                          , x00010405::shift
                                          , x00010405::altGr
                                          , x00010405::shiftAltGr
                                          , x00010405::ctrl
                                          , x00010405::capslock_noMod
                                          , x00010405::capslock_shift
                                          , x00010405::capslock_altGr
                                          , x00010405::capslock_shiftAltGr
                                          , x00010405::deadkeys
                                          , x00010405::nbDeadkeys
);

#endif

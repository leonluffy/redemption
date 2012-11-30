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

#if !defined(__KEYLAYOUT_X0000040A_HPP__)
#define __KEYLAYOUT_X0000040A_HPP__

#include "keylayout.hpp"

namespace x0000040a {    // Spanish (Spain) // Spanish

const static int LCID = 0x0000040a;

const Keylayout::KeyLayout_t noMod = {
    /* x00 - x07 */    0x0000, 0x001B,    '1',    '2',    '3',    '4',    '5',    '6',
    /* x08 - x0F */       '7',    '8',    '9',    '0', 0x0027, 0x00A1, 0x0008, 0x0009,
    /* x10 - x17 */       'q',    'w',    'e',    'r',    't',    'y',    'u',    'i',
    /* x18 - x1F */       'o',    'p', 0x0060, 0x002B, 0x000D, 0x0000,    'a',    's',
    /* x20 - x27 */       'd',    'f',    'g',    'h',    'j',    'k',    'l', 0x00F1,
    /* x28 - x2F */    0x00B4, 0x00BA, 0x0000, 0x00E7,    'z',    'x',    'c',    'v',
    /* x30 - x37 */       'b',    'n',    'm', 0x002C, 0x002E, 0x002D, 0x0000,    '*',
    /* x38 - x3F */    0x0000, 0x0020, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x40 - x47 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,    '7',
    /* x48 - x4F */       '8',    '9',    '-',    '4',    '5',    '6',    '+',    '1',
    /* x50 - x57 */       '2',    '3',    '0', 0x002E, 0x0000, 0x0000, 0x003C, 0x0000,
    /* x58 - x5F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x60 - x67 */    0x0000, 0x0000,    '/', 0x0000, 0x000D, 0x0000, 0x0000, 0x0000,
    /* x68 - x6F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x70 - x77 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x78 - x7F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x002E, 0x0000,
};

const Keylayout::KeyLayout_t shift = {
    /* x00 - x07 */    0x0000, 0x001B, 0x0021, 0x0022, 0x00B7, 0x0024, 0x0025, 0x0026,
    /* x08 - x0F */    0x002F, 0x0028, 0x0029, 0x003D, 0x003F, 0x00BF, 0x0008, 0x0000,
    /* x10 - x17 */       'Q',    'W',    'E',    'R',    'T',    'Y',    'U',    'I',
    /* x18 - x1F */       'O',    'P', 0x005E, 0x002A, 0x000D, 0x0000,    'A',    'S',
    /* x20 - x27 */       'D',    'F',    'G',    'H',    'J',    'K',    'L', 0x00D1,
    /* x28 - x2F */    0x00A8, 0x00AA, 0x0000, 0x00C7,    'Z',    'X',    'C',    'V',
    /* x30 - x37 */       'B',    'N',    'M', 0x003B, 0x003A, 0x005F, 0x0000,    '*',
    /* x38 - x3F */    0x0000, 0x0020, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x40 - x47 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x48 - x4F */    0x0000, 0x0000,    '-', 0x0000, 0x0000, 0x0000,    '+', 0x0000,
    /* x50 - x57 */    0x0000, 0x0000, 0x0000, 0x002E, 0x0000, 0x0000, 0x003E, 0x0000,
    /* x58 - x5F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x60 - x67 */    0x0000, 0x0000, 0x0000, 0x007F, 0x000D, 0x0000, 0x0000, 0x0000,
    /* x68 - x6F */       '/', 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x70 - x77 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x78 - x7F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x002E, 0x0000,
};

const Keylayout::KeyLayout_t altGr = {
    /* x00 - x07 */    0x0000, 0x001B, 0x007C, 0x0040, 0x0023, 0x007E, 0x20AC, 0x00AC,
    /* x08 - x0F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0008, 0x0009,
    /* x10 - x17 */    0x0000, 0x0000, 0x20AC, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x18 - x1F */    0x0000, 0x0000, 0x005B, 0x005D, 0x000D, 0x0000, 0x0000, 0x0000,
    /* x20 - x27 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x28 - x2F */    0x007B, 0x005C, 0x0000, 0x007D, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x30 - x37 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,    '*',
    /* x38 - x3F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x40 - x47 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x48 - x4F */    0x0000, 0x0000,    '-', 0x0000, 0x0000, 0x0000,    '+', 0x0000,
    /* x50 - x57 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x58 - x5F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x60 - x67 */    0x0000, 0x0000,    '/', 0x0000, 0x000D, 0x0000, 0x0000, 0x0000,
    /* x68 - x6F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x70 - x77 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x78 - x7F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
};

const Keylayout::KeyLayout_t shiftAltGr = {
    /* x00 - x07 */    0x0000, 0x001B, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x08 - x0F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0008, 0x0009,
    /* x10 - x17 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x18 - x1F */    0x0000, 0x0000, 0x0000, 0x0000, 0x000D, 0x0000, 0x0000, 0x0000,
    /* x20 - x27 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x28 - x2F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x30 - x37 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,    '*',
    /* x38 - x3F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x40 - x47 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x48 - x4F */    0x0000, 0x0000,    '-', 0x0000, 0x0000, 0x0000,    '+', 0x0000,
    /* x50 - x57 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
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
    /* x18 - x1F */    0x0000, 0x0000, 0x001B, 0x001D, 0x000D, 0x0000, 0x0000, 0x0000,
    /* x20 - x27 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x28 - x2F */    0x0000, 0x0000, 0x0000, 0x001C, 0x0000, 0x0000, 0x0000, 0x0000,
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
    /* x08 - x0F */       '7',    '8',    '9',    '0', 0x0027, 0x00A1, 0x0008, 0x0009,
    /* x10 - x17 */       'Q',    'W',    'E',    'R',    'T',    'Y',    'U',    'I',
    /* x18 - x1F */       'O',    'P', 0x0060, 0x002B, 0x000D, 0x0000,    'A',    'S',
    /* x20 - x27 */       'D',    'F',    'G',    'H',    'J',    'K',    'L', 0x00D1,
    /* x28 - x2F */    0x00B4, 0x00BA, 0x0000, 0x00C7,    'Z',    'X',    'C',    'V',
    /* x30 - x37 */       'B',    'N',    'M', 0x002C, 0x002E, 0x002D, 0x0000,    '*',
    /* x38 - x3F */    0x0000, 0x0020, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x40 - x47 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x48 - x4F */    0x0000, 0x0000,    '-', 0x0000, 0x0000, 0x0000,    '+', 0x0000,
    /* x50 - x57 */    0x0000, 0x0000, 0x0000, 0x002E, 0x0000, 0x0000, 0x003C, 0x0000,
    /* x58 - x5F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x60 - x67 */    0x0000, 0x0000,    '/', 0x0000, 0x000D, 0x0000, 0x0000, 0x0000,
    /* x68 - x6F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x70 - x77 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x78 - x7F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x002E, 0x0000,
};

const Keylayout::KeyLayout_t capslock_shift = {
    /* x00 - x07 */    0x0000, 0x001B, 0x0021, 0x0022, 0x00B7, 0x0024, 0x0025, 0x0026,
    /* x08 - x0F */    0x002F, 0x0028, 0x0029, 0x003D, 0x003F, 0x00BF, 0x0008, 0x0009,
    /* x10 - x17 */       'q',    'w',    'e',    'r',    't',    'y',    'u',    'i',
    /* x18 - x1F */       'o',    'p', 0x005E, 0x002A, 0x000D, 0x0000,    'a',    's',
    /* x20 - x27 */       'd',    'f',    'g',    'h',    'j',    'k',    'l', 0x00F1,
    /* x28 - x2F */    0x00A8, 0x00AA, 0x0000, 0x00E7,    'z',    'x',    'c',    'v',
    /* x30 - x37 */       'b',    'n',    'm', 0x003B, 0x003A, 0x005F, 0x0000,    '*',
    /* x38 - x3F */    0x0000, 0x0020, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x40 - x47 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x48 - x4F */    0x0000, 0x0000,    '-', 0x0000, 0x0000, 0x0000,    '+', 0x0000,
    /* x50 - x57 */    0x0000, 0x0000, 0x0000, 0x002E, 0x0000, 0x0000, 0x003E, 0x0000,
    /* x58 - x5F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x60 - x67 */    0x0000, 0x0000,    '/', 0x0000, 0x000D, 0x0000, 0x0000, 0x0000,
    /* x68 - x6F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x70 - x77 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x78 - x7F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x002E, 0x0000,
};

const Keylayout::KeyLayout_t capslock_altGr = {
    /* x00 - x07 */    0x0000, 0x001B, 0x007C, 0x0040, 0x0023, 0x007E, 0x20AC, 0x00AC,
    /* x08 - x0F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0008, 0x0009,
    /* x10 - x17 */    0x0000, 0x0000, 0x20AC, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x18 - x1F */    0x0000, 0x0000, 0x005B, 0x005D, 0x000D, 0x0000, 0x0000, 0x0000,
    /* x20 - x27 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x28 - x2F */    0x007B, 0x005C, 0x0000, 0x007D, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x30 - x37 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,    '*',
    /* x38 - x3F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x40 - x47 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x48 - x4F */    0x0000, 0x0000,    '-', 0x0000, 0x0000, 0x0000,    '+', 0x0000,
    /* x50 - x57 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x58 - x5F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x60 - x67 */    0x0000, 0x0000,    '/', 0x0000, 0x000D, 0x0000, 0x0000, 0x0000,
    /* x68 - x6F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x70 - x77 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x78 - x7F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
};

const Keylayout::KeyLayout_t capslock_shiftAltGr = {
    /* x00 - x07 */    0x0000, 0x001B, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x08 - x0F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0008, 0x0009,
    /* x10 - x17 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x18 - x1F */    0x0000, 0x0000, 0x0000, 0x0000, 0x000D, 0x0000, 0x0000, 0x0000,
    /* x20 - x27 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x28 - x2F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x30 - x37 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,    '*',
    /* x38 - x3F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x40 - x47 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x48 - x4F */    0x0000, 0x0000,    '-', 0x0000, 0x0000, 0x0000,    '+', 0x0000,
    /* x50 - x57 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x58 - x5F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x60 - x67 */    0x0000, 0x0000,    '/', 0x0000, 0x000D, 0x0000, 0x0000, 0x0000,
    /* x68 - x6F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x70 - x77 */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* x78 - x7F */    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
};

const Keylayout::dkey_t deadkeys[] = {
    { 0x007e, 0x05,  7, { {0x006f, 0x00f5}  // 'o' = 'õ'
                        , {0x0020, 0x007e}  // ' ' = '~'
                        , {0x006e, 0x00f1}  // 'n' = 'ñ'
                        , {0x0041, 0x00c3}  // 'A' = 'Ã'
                        , {0x0061, 0x00e3}  // 'a' = 'ã'
                        , {0x004e, 0x00d1}  // 'N' = 'Ñ'
                        , {0x004f, 0x00d5}  // 'O' = 'Õ'
                        }
    },
    { 0x0060, 0x1a, 11, { {0x006f, 0x00f2}  // 'o' = 'ò'
                        , {0x0049, 0x00cc}  // 'I' = 'Ì'
                        , {0x0020, 0x0060}  // ' ' = '`'
                        , {0x0041, 0x00c0}  // 'A' = 'À'
                        , {0x0045, 0x00c8}  // 'E' = 'È'
                        , {0x0055, 0x00d9}  // 'U' = 'Ù'
                        , {0x0065, 0x00e8}  // 'e' = 'è'
                        , {0x0075, 0x00f9}  // 'u' = 'ù'
                        , {0x0061, 0x00e0}  // 'a' = 'à'
                        , {0x004f, 0x00d2}  // 'O' = 'Ò'
                        , {0x0069, 0x00ec}  // 'i' = 'ì'
                        }
    },
    { 0x005e, 0x1a, 11, { {0x006f, 0x00f4}  // 'o' = 'ô'
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
    { 0x00b4, 0x28, 13, { {0x006f, 0x00f3}  // 'o' = 'ó'
                        , {0x0049, 0x00cd}  // 'I' = 'Í'
                        , {0x0059, 0x00dd}  // 'Y' = 'Ý'
                        , {0x0041, 0x00c1}  // 'A' = 'Á'
                        , {0x0045, 0x00c9}  // 'E' = 'É'
                        , {0x0055, 0x00da}  // 'U' = 'Ú'
                        , {0x0020, 0x00b4}  // ' ' = '´'
                        , {0x0065, 0x00e9}  // 'e' = 'é'
                        , {0x0075, 0x00fa}  // 'u' = 'ú'
                        , {0x0061, 0x00e1}  // 'a' = 'á'
                        , {0x0079, 0x00fd}  // 'y' = 'ý'
                        , {0x004f, 0x00d3}  // 'O' = 'Ó'
                        , {0x0069, 0x00ed}  // 'i' = 'í'
                        }
    },
    { 0x00a8, 0x28, 12, { {0x006f, 0x00f6}  // 'o' = 'ö'
                        , {0x0049, 0x00cf}  // 'I' = 'Ï'
                        , {0x0020, 0x00a8}  // ' ' = '¨'
                        , {0x0041, 0x00c4}  // 'A' = 'Ä'
                        , {0x0045, 0x00cb}  // 'E' = 'Ë'
                        , {0x0055, 0x00dc}  // 'U' = 'Ü'
                        , {0x0065, 0x00eb}  // 'e' = 'ë'
                        , {0x0075, 0x00fc}  // 'u' = 'ü'
                        , {0x0061, 0x00e4}  // 'a' = 'ä'
                        , {0x0079, 0x00ff}  // 'y' = 'ÿ'
                        , {0x004f, 0x00d6}  // 'O' = 'Ö'
                        , {0x0069, 0x00ef}  // 'i' = 'ï'
                        }
    },
};

const static uint8_t nbDeadkeys = 5;

} // END NAMESPACE - x0000040a

static const Keylayout keylayout_x0000040a( x0000040a::LCID
                                          , x0000040a::noMod
                                          , x0000040a::shift
                                          , x0000040a::altGr
                                          , x0000040a::shiftAltGr
                                          , x0000040a::ctrl
                                          , x0000040a::capslock_noMod
                                          , x0000040a::capslock_shift
                                          , x0000040a::capslock_altGr
                                          , x0000040a::capslock_shiftAltGr
                                          , x0000040a::deadkeys
                                          , x0000040a::nbDeadkeys
);

#endif

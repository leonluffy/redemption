
#pragma once

#include "keylayout_r.hpp"

namespace x80000419{ 

const static int LCID = 0x419;

const static char * const locale_name = "ru-RU";

const Keylayout_r::KeyLayoutMap_t noMod
{
	{ 0x001b, 0x1 },
	{ 0x0031, 0x2 },
	{ 0x0032, 0x3 },
	{ 0x0033, 0x4 },
	{ 0x0034, 0x5 },
	{ 0x0035, 0x6 },
	{ 0x0036, 0x7 },
	{ 0x0037, 0x8 },
	{ 0x0038, 0x9 },
	{ 0x0039, 0xa },
	{ 0x0030, 0xb },
	{ 0x002d, 0xc },
	{ 0x003d, 0xd },
	{ 0x0008, 0xe },
	{ 0x0009, 0xf },
	{ 0x0439, 0x10 },
	{ 0x0446, 0x11 },
	{ 0x0443, 0x12 },
	{ 0x043a, 0x13 },
	{ 0x0435, 0x14 },
	{ 0x043d, 0x15 },
	{ 0x0433, 0x16 },
	{ 0x0448, 0x17 },
	{ 0x0449, 0x18 },
	{ 0x0437, 0x19 },
	{ 0x0445, 0x1a },
	{ 0x044a, 0x1b },
	{ 0x000d, 0x1c },
	{ 0x0444, 0x1e },
	{ 0x044b, 0x1f },
	{ 0x0432, 0x20 },
	{ 0x0430, 0x21 },
	{ 0x043f, 0x22 },
	{ 0x0440, 0x23 },
	{ 0x043e, 0x24 },
	{ 0x043b, 0x25 },
	{ 0x0434, 0x26 },
	{ 0x0436, 0x27 },
	{ 0x044d, 0x28 },
	{ 0x0451, 0x29 },
	{ 0x005c, 0x2b },
	{ 0x044f, 0x2c },
	{ 0x0447, 0x2d },
	{ 0x0441, 0x2e },
	{ 0x043c, 0x2f },
	{ 0x0438, 0x30 },
	{ 0x0442, 0x31 },
	{ 0x044c, 0x32 },
	{ 0x0431, 0x33 },
	{ 0x044e, 0x34 },
	{ 0x002e, 0x35 },
	{ 0x002a, 0x37 },
	{ 0x0020, 0x39 },
	{ 0x0037, 0x47 },
	{ 0x0038, 0x48 },
	{ 0x0039, 0x49 },
	{ 0x002d, 0x4a },
	{ 0x0034, 0x4b },
	{ 0x0035, 0x4c },
	{ 0x0036, 0x4d },
	{ 0x002b, 0x4e },
	{ 0x0031, 0x4f },
	{ 0x0032, 0x50 },
	{ 0x0033, 0x51 },
	{ 0x0030, 0x52 },
	{ 0x002c, 0x53 },
	{ 0x005c, 0x56 },
	{ 0x002f, 0x62 },
	{ 0x000d, 0x64 },
	{ 0x002e, 0x7e },
};


const Keylayout_r::KeyLayoutMap_t shift
{
	{ 0x001b, 0x1 },
	{ 0x0021, 0x2 },
	{ 0x0022, 0x3 },
	{ 0x2116, 0x4 },
	{ 0x003b, 0x5 },
	{ 0x0025, 0x6 },
	{ 0x003a, 0x7 },
	{ 0x003f, 0x8 },
	{ 0x002a, 0x9 },
	{ 0x0028, 0xa },
	{ 0x0029, 0xb },
	{ 0x005f, 0xc },
	{ 0x002b, 0xd },
	{ 0x0008, 0xe },
	{ 0x0419, 0x10 },
	{ 0x0426, 0x11 },
	{ 0x0423, 0x12 },
	{ 0x041a, 0x13 },
	{ 0x0415, 0x14 },
	{ 0x041d, 0x15 },
	{ 0x0413, 0x16 },
	{ 0x0428, 0x17 },
	{ 0x0429, 0x18 },
	{ 0x0417, 0x19 },
	{ 0x0425, 0x1a },
	{ 0x042a, 0x1b },
	{ 0x000d, 0x1c },
	{ 0x0424, 0x1e },
	{ 0x042b, 0x1f },
	{ 0x0412, 0x20 },
	{ 0x0410, 0x21 },
	{ 0x041f, 0x22 },
	{ 0x0420, 0x23 },
	{ 0x041e, 0x24 },
	{ 0x041b, 0x25 },
	{ 0x0414, 0x26 },
	{ 0x0416, 0x27 },
	{ 0x042d, 0x28 },
	{ 0x0401, 0x29 },
	{ 0x002f, 0x2b },
	{ 0x042f, 0x2c },
	{ 0x0427, 0x2d },
	{ 0x0421, 0x2e },
	{ 0x041c, 0x2f },
	{ 0x0418, 0x30 },
	{ 0x0422, 0x31 },
	{ 0x042c, 0x32 },
	{ 0x0411, 0x33 },
	{ 0x042e, 0x34 },
	{ 0x002c, 0x35 },
	{ 0x002a, 0x37 },
	{ 0x0020, 0x39 },
	{ 0x002d, 0x4a },
	{ 0x002b, 0x4e },
	{ 0x002c, 0x53 },
	{ 0x002f, 0x56 },
	{ 0x007f, 0x63 },
	{ 0x000d, 0x64 },
	{ 0x002f, 0x68 },
	{ 0x002e, 0x7e },
};


const Keylayout_r::KeyLayoutMap_t altGr
{
	{ 0x001b, 0x1 },
	{ 0x0008, 0xe },
	{ 0x0009, 0xf },
	{ 0x000d, 0x1c },
	{ 0x002a, 0x37 },
	{ 0x002d, 0x4a },
	{ 0x002b, 0x4e },
	{ 0x002f, 0x62 },
	{ 0x000d, 0x64 },
};


const Keylayout_r::KeyLayoutMap_t shiftAltGr
{
	{ 0x001b, 0x1 },
	{ 0x0008, 0xe },
	{ 0x0009, 0xf },
	{ 0x000d, 0x1c },
	{ 0x002a, 0x37 },
	{ 0x002d, 0x4a },
	{ 0x002b, 0x4e },
	{ 0x002f, 0x62 },
	{ 0x000d, 0x64 },
};


const Keylayout_r::KeyLayoutMap_t capslock_noMod
{
	{ 0x001b, 0x1 },
	{ 0x0031, 0x2 },
	{ 0x0032, 0x3 },
	{ 0x0033, 0x4 },
	{ 0x0034, 0x5 },
	{ 0x0035, 0x6 },
	{ 0x0036, 0x7 },
	{ 0x0037, 0x8 },
	{ 0x0038, 0x9 },
	{ 0x0039, 0xa },
	{ 0x0030, 0xb },
	{ 0x002d, 0xc },
	{ 0x003d, 0xd },
	{ 0x0008, 0xe },
	{ 0x0009, 0xf },
	{ 0x0419, 0x10 },
	{ 0x0426, 0x11 },
	{ 0x0423, 0x12 },
	{ 0x041a, 0x13 },
	{ 0x0415, 0x14 },
	{ 0x041d, 0x15 },
	{ 0x0413, 0x16 },
	{ 0x0428, 0x17 },
	{ 0x0429, 0x18 },
	{ 0x0417, 0x19 },
	{ 0x0425, 0x1a },
	{ 0x042a, 0x1b },
	{ 0x000d, 0x1c },
	{ 0x0424, 0x1e },
	{ 0x042b, 0x1f },
	{ 0x0412, 0x20 },
	{ 0x0410, 0x21 },
	{ 0x041f, 0x22 },
	{ 0x0420, 0x23 },
	{ 0x041e, 0x24 },
	{ 0x041b, 0x25 },
	{ 0x0414, 0x26 },
	{ 0x0416, 0x27 },
	{ 0x042d, 0x28 },
	{ 0x0401, 0x29 },
	{ 0x005c, 0x2b },
	{ 0x042f, 0x2c },
	{ 0x0427, 0x2d },
	{ 0x0421, 0x2e },
	{ 0x041c, 0x2f },
	{ 0x0418, 0x30 },
	{ 0x0422, 0x31 },
	{ 0x042c, 0x32 },
	{ 0x0411, 0x33 },
	{ 0x042e, 0x34 },
	{ 0x002e, 0x35 },
	{ 0x002a, 0x37 },
	{ 0x0020, 0x39 },
	{ 0x002d, 0x4a },
	{ 0x002b, 0x4e },
	{ 0x002c, 0x53 },
	{ 0x005c, 0x56 },
	{ 0x002f, 0x62 },
	{ 0x000d, 0x64 },
	{ 0x002e, 0x7e },
};


const Keylayout_r::KeyLayoutMap_t capslock_shift
{
	{ 0x001b, 0x1 },
	{ 0x0021, 0x2 },
	{ 0x0022, 0x3 },
	{ 0x2116, 0x4 },
	{ 0x003b, 0x5 },
	{ 0x0025, 0x6 },
	{ 0x003a, 0x7 },
	{ 0x003f, 0x8 },
	{ 0x002a, 0x9 },
	{ 0x0028, 0xa },
	{ 0x0029, 0xb },
	{ 0x005f, 0xc },
	{ 0x002b, 0xd },
	{ 0x0008, 0xe },
	{ 0x0009, 0xf },
	{ 0x0439, 0x10 },
	{ 0x0446, 0x11 },
	{ 0x0443, 0x12 },
	{ 0x043a, 0x13 },
	{ 0x0435, 0x14 },
	{ 0x043d, 0x15 },
	{ 0x0433, 0x16 },
	{ 0x0448, 0x17 },
	{ 0x0449, 0x18 },
	{ 0x0437, 0x19 },
	{ 0x0445, 0x1a },
	{ 0x044a, 0x1b },
	{ 0x000d, 0x1c },
	{ 0x0444, 0x1e },
	{ 0x044b, 0x1f },
	{ 0x0432, 0x20 },
	{ 0x0430, 0x21 },
	{ 0x043f, 0x22 },
	{ 0x0440, 0x23 },
	{ 0x043e, 0x24 },
	{ 0x043b, 0x25 },
	{ 0x0434, 0x26 },
	{ 0x0436, 0x27 },
	{ 0x044d, 0x28 },
	{ 0x0451, 0x29 },
	{ 0x002f, 0x2b },
	{ 0x044f, 0x2c },
	{ 0x0447, 0x2d },
	{ 0x0441, 0x2e },
	{ 0x043c, 0x2f },
	{ 0x0438, 0x30 },
	{ 0x0442, 0x31 },
	{ 0x044c, 0x32 },
	{ 0x0431, 0x33 },
	{ 0x044e, 0x34 },
	{ 0x002c, 0x35 },
	{ 0x002a, 0x37 },
	{ 0x0020, 0x39 },
	{ 0x002d, 0x4a },
	{ 0x002b, 0x4e },
	{ 0x002c, 0x53 },
	{ 0x002f, 0x56 },
	{ 0x002f, 0x62 },
	{ 0x000d, 0x64 },
	{ 0x002e, 0x7e },
};


const Keylayout_r::KeyLayoutMap_t capslock_altGr
{
	{ 0x001b, 0x1 },
	{ 0x0008, 0xe },
	{ 0x0009, 0xf },
	{ 0x000d, 0x1c },
	{ 0x002a, 0x37 },
	{ 0x002d, 0x4a },
	{ 0x002b, 0x4e },
	{ 0x002f, 0x62 },
	{ 0x000d, 0x64 },
};


const Keylayout_r::KeyLayoutMap_t capslock_shiftAltGr
{
	{ 0x001b, 0x1 },
	{ 0x0008, 0xe },
	{ 0x0009, 0xf },
	{ 0x000d, 0x1c },
	{ 0x002a, 0x37 },
	{ 0x002d, 0x4a },
	{ 0x002b, 0x4e },
	{ 0x002f, 0x62 },
	{ 0x000d, 0x64 },
};


const Keylayout_r::KeyLayoutMap_t ctrl
{
	{ 0x001b, 0x1 },
	{ 0x0008, 0xe },
	{ 0x0009, 0xf },
	{ 0x000d, 0x1c },
	{ 0x001c, 0x2b },
	{ 0x002a, 0x37 },
	{ 0x0020, 0x39 },
	{ 0x002d, 0x4a },
	{ 0x002b, 0x4e },
	{ 0x001c, 0x56 },
	{ 0x002f, 0x62 },
	{ 0x000d, 0x64 },
};


const Keylayout_r::KeyLayoutMap_t deadkeys
{
};


const static uint8_t nbDeadkeys = 0;

}

static const Keylayout_r keylayout_x80000419( x80000419::LCID
                                 , x80000419::locale_name
                                 , x80000419::noMod
                                 , x80000419::shift
                                 , x80000419::altGr
                                 , x80000419::shiftAltGr
                                 , x80000419::ctrl
                                 , x80000419::capslock_noMod
                                 , x80000419::capslock_shift
                                 , x80000419::capslock_altGr
                                 , x80000419::capslock_shiftAltGr
                                 , x80000419::deadkeys
                                 , x80000419::nbDeadkeys

);


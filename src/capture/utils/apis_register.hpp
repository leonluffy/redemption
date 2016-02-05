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
*   Copyright (C) Wallix 2010-2016
*   Author(s): Jonathan Poelen
*/

#ifndef REDEMPTION_CAPTURE_UTILS_APIS_REGISTER_HPP
#define REDEMPTION_CAPTURE_UTILS_APIS_REGISTER_HPP

#include <vector>
#include <functional>

namespace gdi {
  class GraphicApi;
  class CaptureApi;
  class CaptureProbeApi;
  class InputKbdApi;
  class InputPointer;
}

struct ApisRegister
{
    std::vector<std::reference_wrapper<gdi::GraphicApi>> * graphic_list;
    std::vector<std::reference_wrapper<gdi::CaptureApi>> * graphic_snapshot_list;
    std::vector<std::reference_wrapper<gdi::CaptureApi>> & capture_list;
    std::vector<std::reference_wrapper<gdi::InputKbdApi>> & input_kbd_list;
    std::vector<std::reference_wrapper<gdi::InputPointer>> & input_pointer_list;
    std::vector<std::reference_wrapper<gdi::CaptureProbeApi>> & capture_probe_list;
};

#endif

/*
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *   Product name: redemption, a FLOSS RDP proxy
 *   Copyright (C) Wallix 2010-2019
 *   Author(s): Meng Tan
 */


#include "mod/internal/transition_mod.hpp"
#include "mod/internal/widget/tooltip.hpp"
#include "keyboard/keymap2.hpp"
#include "configs/config.hpp"
#include "core/front_api.hpp"

TransitionMod::TransitionMod(
    TransitionModVariables vars,
    FrontAPI & front, uint16_t width, uint16_t height,
    Rect const widget_rect, ClientExecute & rail_client_execute
)
    : LocallyIntegrableMod(front, width, height,
                           vars.get<cfg::font>(), rail_client_execute,
                           vars.get<cfg::theme>())
    , ttmessage(front, this->screen, nullptr,
                TR(trkeys::wait_msg, language(vars)),
                vars.get<cfg::theme>().tooltip.fgcolor,
                vars.get<cfg::theme>().tooltip.bgcolor,
                vars.get<cfg::theme>().tooltip.border_color,
                vars.get<cfg::font>())
    , vars(vars)
{
    Dimension dim;
    dim = this->ttmessage.get_optimal_dim();
    this->ttmessage.set_wh(dim);
    this->ttmessage.set_xy(widget_rect.x + (widget_rect.cx - dim.w) / 2,
                           widget_rect.y + (widget_rect.cy - dim.h) / 2);
    this->ttmessage.rdp_input_invalidate(this->ttmessage.get_rect());
}

TransitionMod::~TransitionMod()
{
    this->screen.clear();
}

void TransitionMod::rdp_input_scancode(long int, long int, long int,
                                       long int, Keymap2* keymap)
{
    if (keymap->nb_kevent_available() > 0){
        switch (keymap->top_kevent()){
        case Keymap2::KEVENT_ESC:
            keymap->get_kevent();
            this->event.signal = BACK_EVENT_STOP;
            this->event.set_trigger_time(wait_obj::NOW);
            break;
        default:;
        }
    }
}

void TransitionMod::draw_event(time_t now, gdi::GraphicApi & gapi)
{
    LocallyIntegrableMod::draw_event(now, gapi);
}

void TransitionMod::send_to_mod_channel(CHANNELS::ChannelNameId front_channel_name, InStream& chunk, size_t length, uint32_t flags)
{
    LocallyIntegrableMod::send_to_mod_channel(front_channel_name, chunk, length, flags);
}

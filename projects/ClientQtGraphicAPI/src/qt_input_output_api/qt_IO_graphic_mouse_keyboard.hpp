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
   Author(s): Clément Moroldo, Jonathan Poelen, Christophe Grosjean, David Fort
*/



#pragma once

#include "core/RDP/RDPDrawable.hpp"
#include "utils/log.hpp"

#include "keymaps/qt_scancode_keymap.hpp"

#include "qt_graphics_components/qt_progress_bar_window.hpp"
#include "qt_graphics_components/qt_options_window.hpp"
#include "qt_graphics_components/qt_screen_window.hpp"
#include "qt_graphics_components/qt_form_window.hpp"

#include "client_redemption/client_input_output_api/client_graphic_api.hpp"
#include "client_redemption/client_input_output_api/client_mouse_keyboard_api.hpp"

#include <QtGui/QBitmap>
#include <QtGui/QColor>
#include <QtGui/QImage>
#include <QtGui/QMouseEvent>
#include <QtGui/QPainter>
#include <QtGui/QRgb>
#include <QtGui/QWheelEvent>
// #include <QtGui/QWindowsCEStyle>

#if REDEMPTION_QT_VERSION == 4
#   include <QtCore/QUrl>
#   define REDEMPTION_QT_INCLUDE_WIDGET(name) <QtGui/name>
#else
#   define REDEMPTION_QT_INCLUDE_WIDGET(name) <QtWidgets/name>
#endif

#include REDEMPTION_QT_INCLUDE_WIDGET(QApplication)
#include REDEMPTION_QT_INCLUDE_WIDGET(QDesktopWidget)

#undef REDEMPTION_QT_INCLUDE_WIDGET


void update_cache(Rect clip, RDPDrawable& drawable, QPainter& painter);

class QtIOGraphicMouseKeyboard : public ClientOutputGraphicAPI, public ClientInputMouseKeyboardAPI
{
    QtForm             * form = nullptr;
    QtScreen           * screen = nullptr;
    QPixmap              cache;
    ProgressBarWindow  * bar = nullptr;
    QPainter             painter;
    QImage cursor_image;
    std::map<uint32_t, RemoteAppQtScreen *> remote_app_screen_map;
    //     QPixmap            * trans_cache;
    RDPDrawable drawable;
    Qt_ScanCode_KeyMap   qtRDPKeymap;

    std::vector<QPixmap> balises;

public:
    QtIOGraphicMouseKeyboard()
      : ClientOutputGraphicAPI(QApplication::desktop()->width(), QApplication::desktop()->height())
      // QImage: each scanline of data in the image must be 32-bit aligned.
      , drawable(this->screen_max_width, this->screen_max_height)
    {}


    ////////////////////////////////////////////////////////////////////////////////////////////////////////
    //-----------------------------
    // MAIN WINDOW MANAGEMENT FUNCTIONS
    //-----------------------------

    void set_drawn_client(ClientRedemptionAPI * client, ClientRedemptionConfig * config) override {
        this->drawn_client = client;
        this->config = config;
        //this->qtRDPKeymap._verbose = (this->drawn_client->verbose == RDPVerbose::input) ? 1 : 0;
        this->qtRDPKeymap.setKeyboardLayout(this->config->info.keylayout);

        this->qtRDPKeymap.clearCustomKeyCode();
        for (size_t i = 0; i < this->drawn_client->keyCustomDefinitions.size(); i++) {
            ClientRedemptionAPI::KeyCustomDefinition & key = this->drawn_client->keyCustomDefinitions[i];
            this->qtRDPKeymap.setCustomKeyCode(key.qtKeyID, key.scanCode, key.ASCII8, key.extended);
        }

        this->form = new QtForm(this->config, this, this->client);
    }

    void show_screen() override {
        if (this->form) {
            this->form->hide();
            if (this->screen) {
                this->screen->show();
            }
        }
    }

    void set_screen_size(int x, int y) override {
        if (this->screen) {
            this->screen->setFixedSize(x, y);
        }
    }

    void update_screen() override {
        if (this->screen) {
            this->screen->slotRepainMatch();
        }
    }

    void reset_cache(const int w,  const int h) override {

        LOG(LOG_INFO, "reset_cache w=%d h=%d", w, h);
        if (w == 0 || h == 0) {
            return;
        }
        if (this->painter.isActive()) {
            this->painter.end();
        }

        this->cache = QPixmap(w, h);
        this->drawable.resize(align4(w), h);

        LOG(LOG_INFO, "reset_cache this->cache.width()=%d this->cache.height()=%d", this->cache.width(), this->cache.height());

        if (!(this->cache.isNull())) {
            this->painter.begin(&this->cache);
        }

        this->painter.fillRect(0, 0, w, h, Qt::black);
    }

    void create_screen() override {
        this->screen = new RDPQtScreen(this->config, this->drawn_client, this, &this->cache);
    }

    void create_screen(std::string const & movie_dir, std::string const & movie_path) override {
        this->screen = new ReplayQtScreen(this->config, this->drawn_client, this, movie_dir, movie_path, &this->cache, this->client->get_movie_time_length(this->client->get_mwrm_filename()), 0);
    }

    QWidget * get_static_qwidget() {
        return this->form;
    }

    void open_options() override {
//         new DialogOptions_Qt(this->drawn_client, this->form);
        if (this->form) {
            this->form->options();
        }
    }

    void dropScreen() override {
        if (this->screen != nullptr) {
            this->screen->disconnection();
            this->screen = nullptr;
        }
    }

    void update_keylayout() override {
        this->qtRDPKeymap.setKeyboardLayout(this->config->info.keylayout);
    }

    void readError(std::string const & movie_path) {
        const std::string errorMsg("Cannot read movie \""+movie_path+ "\".");
        LOG(LOG_INFO, "%s", errorMsg.c_str());
        std::string labelErrorMsg("<font color='Red'>"+errorMsg+"</font>");

        if (this->form) {
            this->form->set_ErrorMsg(labelErrorMsg);
        }
    }

    void set_ErrorMsg(std::string const & error_msg) override {
        if (this->form) {
            this->form->set_ErrorMsg(error_msg);
        }
    }

    void init_form() override {
        if (this->form) {
            if (this->config->mod_state != ClientRedemptionAPI::MOD_RDP_REPLAY) {
                this->form->init_form();
                this->form->set_IPField(this->client->target_IP);
                this->form->set_portField(this->client->port);
                this->form->set_PWDField(this->client->user_password);
                this->form->set_userNameField(this->client->user_name);
            }
            this->form->show();
        }
    }



    /////////////////////////////////////////////////////////////////////////////////////////////////
    //  REMOTE APP FUNCTIONS
    //////////////////////////

    void create_remote_app_screen(uint32_t id, int w, int h, int x, int y) override {
        LOG(LOG_INFO, "create_remote_app_screen 1");
        this->remote_app_screen_map.insert(std::pair<uint32_t, RemoteAppQtScreen *>(id, nullptr));
        this->remote_app_screen_map[id] = new RemoteAppQtScreen(this->config, this->drawn_client, this, w, h, x, y, &this->cache);
        LOG(LOG_INFO, "create_remote_app_screen 2");
    }

    void show_screen(uint32_t id) override {
        if (this->form) {
            this->form->hide();
            if (this->remote_app_screen_map[id]) {
                this->remote_app_screen_map[id]->show();
            }
        }
    }

    void move_screen(uint32_t id, int x, int y) override {
        if (this->remote_app_screen_map[id]) {
            this->remote_app_screen_map[id]->move(x, y);
        }
    }

    void set_screen_size(uint32_t id, int x, int y) override {
        if (this->remote_app_screen_map[id]) {
            this->remote_app_screen_map[id]->setFixedSize(x, y);
        }
    }

    void set_pixmap_shift(uint32_t id, int x, int y) override {
        if (this->remote_app_screen_map[id]) {
            this->remote_app_screen_map[id]->x_pixmap_shift = x;
            this->remote_app_screen_map[id]->y_pixmap_shift = y;
        }
    }

    int get_visible_width(uint32_t id) override {
        return this->remote_app_screen_map[id]->width();
    }

    int get_visible_height(uint32_t id) override {
        return this->remote_app_screen_map[id]->height();
    }

    int get_mem_width(uint32_t id) override {
        return this->remote_app_screen_map[id]->_width;
    }

    int get_mem_height(uint32_t id) override {
        return this->remote_app_screen_map[id]->_height;
    }

    void set_mem_size(uint32_t id, int w, int h) override {
        this->remote_app_screen_map[id]->_width = w;
        this->remote_app_screen_map[id]->_height = h;
    }

    void dropScreen(uint32_t id) override {
        if (this->remote_app_screen_map[id] != nullptr) {
            this->remote_app_screen_map[id]->disconnection();
            this->remote_app_screen_map[id] = nullptr;
        }

        std::map<uint32_t, RemoteAppQtScreen *>::iterator it = remote_app_screen_map.find(id);
        remote_app_screen_map.erase (it);
    }

    void clear_remote_app_screen() override {
        for (std::map<uint32_t, RemoteAppQtScreen *>::iterator it=this->remote_app_screen_map.begin(); it!=this->remote_app_screen_map.end(); ++it) {
            if (it->second) {
                it->second->disconnection();
                it->second = nullptr;
            }
        }
        this->remote_app_screen_map.clear();

        if (this->form) {
            this->form->show();
        }
    }



//      void setScreenDimension() {
//         if (!this->is_spanning) {
//             this->_screen_dimensions[0].cx = this->config->info.width;
//             this->_screen_dimensions[0].cy = this->config->info.height;
//
//         } else {
//
//             QDesktopWidget* desktop = QApplication::desktop();
//             int screen_count(desktop->screenCount());
//             if (this->_monitorCount > screen_count) {
//                 this->_monitorCount = screen_count;
//             }
//             this->config->info.width  = 0;
//             this->config->info.height = 0;
//             this->config->info.cs_monitor.monitorCount = this->_monitorCount;
//
//             for (int i = 0; i < this->_monitorCount; i++) {
//                 const QRect rect = desktop->screenGeometry(i);
//                 this->_screen_dimensions[i].x   = this->config->info.width;
//                 this->config->info.cs_monitor.monitorDefArray[i].left   = this->config->info.width;
//                 this->config->info.width  += rect.width();
//
//                 if (this->config->info.height < rect.height()) {
//                     this->config->info.height = rect.height();
//                 }
//                 this->config->info.cs_monitor.monitorDefArray[i].top    = rect.top();
//                 this->config->info.cs_monitor.monitorDefArray[i].right  = this->config->info.width + rect.width() - 1;
//                 this->config->info.cs_monitor.monitorDefArray[i].bottom = rect.height() - 1 - 3*Screen_Qt::BUTTON_HEIGHT;
//
//                 this->config->info.cs_monitor.monitorDefArray[i].flags  = 0;
//
//                 this->_screen_dimensions[i].y   = 0;
//                 this->_screen_dimensions[i].cx  = rect.width();
//                 this->_screen_dimensions[i].cy  = rect.height() - 3*Screen_Qt::BUTTON_HEIGHT;
//             }
//             this->config->info.cs_monitor.monitorDefArray[0].flags  = GCC::UserData::CSMonitor::TS_MONITOR_PRIMARY;
//             this->config->info.height -= 3*Screen_Qt::BUTTON_HEIGHT;
//         }
//     }

//      void setClip(int x, int y, int w, int h) {
//
//          if (this->screen) {
//
//              if (this->screen->clip.x() == -1) {
//                  this->screen->clip.setX(x);
//                  this->screen->clip.setY(y);
//                  this->screen->clip.setWidth(w);
//                  this->screen->clip.setHeight(h);
//              } else {
//                  const int ori_x = this->screen->clip.x();
//                  const int ori_y = this->screen->clip.y();
//
//                  if (x <= ori_x) {
//                      this->screen->clip.setX(x);
//                  }
//
//                  if (y <= ori_y) {
//                      this->screen->clip.setY(y);
//                  }
//
//                  if ( (x+w) > (ori_x + this->screen->clip.width()) ) {
//                      this->screen->clip.setWidth(x+w-this->screen->clip.x());
//                  }
//
//                  if ( (y+h) > (ori_y + this->screen->clip.height()) ) {
//                      this->screen->clip.setHeight(y+h-this->screen->clip.y());
//                  }
//              }
//          }
//     }

    void begin_update() override {

        this->update_counter++;
    }

private:
    size_t update_counter = 0;

    void end_update() override {
        assert(this->update_counter);
        this->update_counter--;
        if (this->update_counter != 0){
            return;
        }

        if (this->config->mod_state == ClientRedemptionAPI::MOD_RDP_REMOTE_APP) {
            for (std::map<uint32_t, RemoteAppQtScreen *>::iterator it=this->remote_app_screen_map.begin(); it!=this->remote_app_screen_map.end(); ++it) {
                if (it->second) {
                    it->second->update_view();
                }
            }
        } else {
            if (this->screen != nullptr) {
                this->screen->update_view();
            }
        }
    }

    FrontAPI::ResizeResult server_resize(const int width, const int height, const int bpp) override {

        if (width == 0 || height == 0) {
            return FrontAPI::ResizeResult::fail;
        }

        switch (this->config->mod_state) {

            case ClientRedemptionAPI::MOD_RDP:
                if (this->config->info.width == width && this->config->info.height == height) {
                    return FrontAPI::ResizeResult::instant_done;
                }
                this->dropScreen();
                this->reset_cache(width, height);
                this->screen = new RDPQtScreen(this->config, this->drawn_client, this, &this->cache);
                this->screen->show();
                    break;

            case ClientRedemptionAPI::MOD_VNC:
                if (this->client->vnc_conf.width == width && this->client->vnc_conf.height == height) {
                    return FrontAPI::ResizeResult::instant_done;
                }
                this->client->vnc_conf.width = width;
                this->client->vnc_conf.height = height;
                this->dropScreen();
                this->reset_cache(width, height);
                this->screen = new RDPQtScreen(this->config, this->drawn_client, this, &this->cache);
                this->screen->show();
                    break;

            case ClientRedemptionAPI::MOD_RDP_REMOTE_APP:
                return FrontAPI::ResizeResult::remoteapp;
                    break;

            case ClientRedemptionAPI::MOD_RDP_REPLAY:
                if (!this->client->is_loading_replay_mod) {
                    time_t current_time_movie = 0;

                    if (!this->is_pre_loading) {
                        if (this->screen) {
                            current_time_movie = this->screen->get_current_time_movie();
                        }
                        this->dropScreen();
                    }
                    this->reset_cache(width, height);

                    if (!this->is_pre_loading) {
                        this->screen = new ReplayQtScreen(this->config, this->drawn_client, this, this->client->_movie_dir, this->client->_movie_name, &this->cache, this->client->get_movie_time_length(this->client->get_mwrm_filename()), current_time_movie);

                        this->screen->show();
                    }
                }
                return FrontAPI::ResizeResult::instant_done;
                    break;
        }

        this->config->info.bpp = bpp;

        return FrontAPI::ResizeResult::instant_done;
    }

    void set_pointer(Pointer const & cursor) override {

        auto dimensions = cursor.get_dimensions();
        auto hotspot = cursor.get_hotspot();

        ARGB32Pointer vnccursor(cursor);
        const auto av_alpha_q = vnccursor.get_alpha_q();

        //::hexdump(av_alpha_q.data(), dimensions.width * dimensions.height, dimensions.width);

        // this->cursor_image is used when client is replaying
        this->cursor_image = QImage(av_alpha_q.data(), dimensions.width, dimensions.height, dimensions.width * 4, QImage::Format_ARGB32_Premultiplied);



        if (this->config->mod_state == ClientRedemptionAPI::MOD_RDP_REMOTE_APP) {
            for (std::map<uint32_t, RemoteAppQtScreen *>::iterator it=this->remote_app_screen_map.begin(); it!=this->remote_app_screen_map.end(); ++it) {
                if (it->second) {
                    it->second->setCursor(QCursor(QPixmap::fromImage(this->cursor_image), hotspot.x, hotspot.x));
                }
            }
        } else if (this->screen) {
            this->screen->setCursor(QCursor(QPixmap::fromImage(this->cursor_image), hotspot.x, hotspot.x));
        }
    }


    void pre_load_movie() override {

        this->balises.clear();

        long int movie_length = this->client->get_movie_time_length(this->client->get_mwrm_filename());
        this->form->hide();
        this->bar = new ProgressBarWindow(movie_length);
        long int endin_frame = 0;

        this->is_pre_loading = true;

        if (movie_length > ClientRedemptionAPI::BALISED_FRAME) {

            while (endin_frame < movie_length) {

                this->client->instant_play_client(std::chrono::microseconds(endin_frame*1000000));

                this->balises.push_back(this->cache);
                endin_frame += ClientRedemptionAPI::BALISED_FRAME;
                if (this->bar) {
                    this->bar->setValue(endin_frame);
                }
            }
        }

        this->is_pre_loading = false;

        if (this->screen) {
            this->screen->stopRelease();
        }
    }

//     void answer_question(int color) {
//         QImage image = this->cache.toImage();
//
//         QRgb asked(color);
//
//         QRgb top_left  = image.pixel(0, 0);
//         QRgb top_right = image.pixel(this->config->info.width-1, 0);
//         QRgb bot_left  = image.pixel(0, this->config->info.height-1);
//         QRgb bot_right = image.pixel(this->config->info.width-1, this->config->info.height-1);
//
//         //LOG(LOG_INFO, "         top_left = 0x%04x top_right = 0x%04x bot_left = 0x%04x bot_right = 0x%04x asked_color = 0x%04x, top_left, top_right, bot_left, bot_right, asked);
//
//         if        (top_left == asked) {
//             this->mod->rdp_input_mouse(MOUSE_FLAG_BUTTON1 | MOUSE_FLAG_DOWN, 0, 0, &(this->keymap));
//             this->mod->rdp_input_mouse(MOUSE_FLAG_BUTTON1, 0, 0, &(this->keymap));
//             this->wab_diag_question = false;
// //             LOG(LOG_INFO, "CLIENT >> answer_question top_left");
//
//         } else if (top_right == asked) {
//             this->mod->rdp_input_mouse(MOUSE_FLAG_BUTTON1 | MOUSE_FLAG_DOWN, this->config->info.width-1, 0, &(this->keymap));
//             this->mod->rdp_input_mouse(MOUSE_FLAG_BUTTON1, this->config->info.width-1, 0, &(this->keymap));
//             this->wab_diag_question = false;
// //             LOG(LOG_INFO, "CLIENT >> answer_question top_right");
//
//         } else if (bot_left == asked) {
//             this->mod->rdp_input_mouse(MOUSE_FLAG_BUTTON1 | MOUSE_FLAG_DOWN, 0, this->config->info.height-1, &(this->keymap));
//             this->mod->rdp_input_mouse(MOUSE_FLAG_BUTTON1, 0, this->config->info.height-1, &(this->keymap));
//             this->wab_diag_question = false;
// //             LOG(LOG_INFO, "CLIENT >> answer_question bot_left");
//
//         } else if (bot_right == asked) {
//             this->mod->rdp_input_mouse(MOUSE_FLAG_BUTTON1 | MOUSE_FLAG_DOWN, this->config->info.width-1, this->config->info.height-1, &(this->keymap));
//             this->mod->rdp_input_mouse(MOUSE_FLAG_BUTTON1, this->config->info.width-1, this->config->info.height-1, &(this->keymap));
//             this->wab_diag_question = false;
// //             LOG(LOG_INFO, "CLIENT >> answer_question bot_right");
//
//         }
//     }


private:
    template<class... Ts>
    void draw_impl(Rect clip, Ts&&... xs)
    {
        if (this->client->connected || this->client->is_replaying) {
            clip = clip.intersect(drawable.get());
            if (clip.cy && clip.cx) {
                this->drawable.draw(xs...);
                update_cache(clip, this->drawable, this->painter);
            }
        }
    }

public:
    void draw(const RDPPatBlt & cmd, Rect clip, gdi::ColorCtx color_ctx) override
    {
        this->draw_impl(clip, cmd, clip, color_ctx);
    }


    void draw(const RDPOpaqueRect & cmd, Rect clip, gdi::ColorCtx color_ctx) override
    {
        this->draw_impl(clip, cmd, clip, color_ctx);
    }


    void draw(const RDPBitmapData & bitmap_data, const Bitmap & bmp) override
    {
        if (!bmp.is_valid()){
            return;
        }

        const Rect dest( bitmap_data.dest_left, bitmap_data.dest_top
                       , bitmap_data.dest_right - bitmap_data.dest_left + 1
                       , bitmap_data.dest_bottom - bitmap_data.dest_top + 1);

        this->draw_impl(dest, bitmap_data, bmp);
    }


    void draw(const RDPLineTo & cmd, Rect clip, gdi::ColorCtx color_ctx) override
    {
        this->draw_impl(clip, cmd, clip, color_ctx);
    }


    void draw(const RDPScrBlt & cmd, Rect clip) override
    {
        this->draw_impl(clip, cmd, clip);
    }


    void draw(const RDPMemBlt & cmd, Rect clip, const Bitmap & bitmap) override
    {
        this->draw_impl(clip, cmd, clip, bitmap);
    }


    void draw(const RDPMem3Blt & cmd, Rect clip, gdi::ColorCtx color_ctx, const Bitmap & bitmap) override
    {
        this->draw_impl(clip, cmd, clip, color_ctx, bitmap);
    }


    void draw(const RDPDestBlt & cmd, Rect clip) override
    {
        this->draw_impl(clip, cmd, clip);
    }

    void draw(const RDPMultiDstBlt & cmd, Rect clip) override
    {
        this->draw_impl(clip, cmd, clip);
    }

    void draw(const RDPMultiOpaqueRect & cmd, Rect clip, gdi::ColorCtx color_ctx) override
    {
        this->draw_impl(clip, cmd, clip, color_ctx);
    }

    void draw(const RDP::RDPMultiPatBlt & cmd, Rect clip, gdi::ColorCtx color_ctx) override
    {
        this->draw_impl(clip, cmd, clip, color_ctx);
    }

    void draw(const RDP::RDPMultiScrBlt & cmd, Rect clip) override
    {
        this->draw_impl(clip, cmd, clip);
    }

    void draw(const RDPGlyphIndex & cmd, Rect clip, gdi::ColorCtx color_ctx, const GlyphCache & gly_cache) override
    {
        this->draw_impl(clip, cmd, clip, color_ctx, gly_cache);
    }

    void draw(const RDPPolygonSC & cmd, Rect clip, gdi::ColorCtx color_ctx) override
    {
        this->draw_impl(clip, cmd, clip, color_ctx);
    }

    void draw(const RDPPolygonCB & cmd, Rect clip, gdi::ColorCtx color_ctx) override
    {
        this->draw_impl(clip, cmd, clip, color_ctx);
    }

    void draw(const RDPPolyline & cmd, Rect clip, gdi::ColorCtx color_ctx) override
    {
        this->draw_impl(clip, cmd, clip, color_ctx);
    }

    void draw(const RDPEllipseSC & cmd, Rect clip, gdi::ColorCtx color_ctx) override
    {
        this->draw_impl(clip, cmd, clip, color_ctx);
    }

    void draw(const RDPEllipseCB & cmd, Rect clip, gdi::ColorCtx color_ctx) override
    {
        this->draw_impl(clip, cmd, clip, color_ctx);
    }

    void draw(RDPNineGrid const & cmd, Rect clip, gdi::ColorCtx color_ctx, Bitmap const & bmp) override
    {
        this->draw_impl(clip, cmd, clip, color_ctx, bmp);
    }

    void draw(const RDP::FrameMarker & order) override
    {
        (void) order;
        // LOG(LOG_INFO, "DEFAULT: FrameMarker");
    }

    using ClientOutputGraphicAPI::draw;


    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //------------------------
    //      CONTROLLERS
    //------------------------

    void keyPressEvent(const int key, std::string const& text) override {
//         if (this->client->mod_state ==  ClientRedemptionAPI::MOD_VNC) {
//             this->client->send_rdp_unicode(text, 0);
//         } else {
        this->qtRDPKeymap.keyEvent(0, key, text);
        if (this->qtRDPKeymap.scanCode != 0) {
            this->client->send_rdp_scanCode(this->qtRDPKeymap.scanCode, this->qtRDPKeymap.flag);
        }
//         }
    }

    void keyReleaseEvent(const int key, std::string const& text) override {
//          if (this->client->mod_state ==  ClientRedemptionAPI::MOD_VNC) {
//             this->client->send_rdp_unicode(text, KBD_FLAG_UP);
//         } else {
            this->qtRDPKeymap.keyEvent(KBD_FLAG_UP, key, text);
            if (this->qtRDPKeymap.scanCode != 0) {
                this->client->send_rdp_scanCode(this->qtRDPKeymap.scanCode, this->qtRDPKeymap.flag);
            }
//         }
    }

    bool connexionReleased() override {
        if (this->form) {
            this->form->setCursor(Qt::WaitCursor);
            this->client->user_name     = this->form->get_userNameField();
            this->client->target_IP     = this->form->get_IPField();
            this->client->user_password = this->form->get_PWDField();
            this->client->port          = this->form->get_portField();

            this->client->is_full_capturing = true;
            this->client->full_capture_file_name = "/tmp/capture.dump";

            bool conn_res = false;
            if (!this->client->target_IP.empty()){
                conn_res = this->client->connect();
            }
            this->form->setCursor(Qt::ArrowCursor);

            return conn_res;
        }

        return false;
    }

    void closeFromScreen() override {

        this->disconnexionReleased();

        if (this->form != nullptr && this->client->connected) {
            this->form->close();
        }
    }

    ClientRedemptionAPI::KeyCustomDefinition get_key_info(int key, std::string const& text) override {
        this->qtRDPKeymap.keyEvent(0, key, text);
        ClientRedemptionAPI::KeyCustomDefinition key_info(
            this->qtRDPKeymap.qKeyCode,
            this->qtRDPKeymap.scanCode,
            this->qtRDPKeymap.ascii,
            this->qtRDPKeymap.flag &0x0100 ? 0x0100: 0,
            this->qtRDPKeymap.qKeyName
          );

       // key_info.name = this->qtRDPKeymap.name;

        return key_info;
    }

};

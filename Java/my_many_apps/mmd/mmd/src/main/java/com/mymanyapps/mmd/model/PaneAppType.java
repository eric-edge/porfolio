/*******************************************************************************
 * MIT License
 *
 * Copyright (c) 2019 Eric Thivierge
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * Contributors:
 *     Eric Thivierge
 *******************************************************************************/
package com.mymanyapps.mmd.model;

import com.mymanyapps.mma.model.AppFactory;
import com.mymanyapps.mma.model.paneapp.AbstractPaneApp;
import com.mymanyapps.mma.view.AbstractAppView;
import com.mymanyapps.mmd.app.ClockPaneApp;
import com.mymanyapps.mmd.app.LabelPaneApp;
import com.mymanyapps.mmd.app.RedisCommandApp;
import com.mymanyapps.mmd.view.ClockPaneAppView;
import com.mymanyapps.mmd.view.LabelPaneAppView;
import com.mymanyapps.mmd.view.RedisCommandView;

public enum PaneAppType
    implements AppFactory
{
    CLOCK
    {
        @Override
        public AbstractPaneApp<?> buildApp()
        {
            return new ClockPaneApp();
        }

        @Override
        public AbstractAppView<?> buildView(AbstractPaneApp<?> paneApp)
        {
            ClockPaneAppView view = new ClockPaneAppView((ClockPaneApp) paneApp);
            ((ClockPaneApp) paneApp).inject(view);

            return view;
        }
    },
    DISPLAY_LABEL
    {
        @Override
        public AbstractPaneApp<?> buildApp()
        {
            return new LabelPaneApp();
        }

        @Override
        public AbstractAppView<?> buildView(AbstractPaneApp<?> paneApp)
        {
            LabelPaneAppView view = new LabelPaneAppView((LabelPaneApp) paneApp);
            ((LabelPaneApp) paneApp).inject(view);

            return view;
        }
    },
    REDIS_COMMAND
    {
        @Override
        public AbstractPaneApp<?> buildApp()
        {
            return new RedisCommandApp();
        }

        @Override
        public AbstractAppView<?> buildView(AbstractPaneApp<?> paneApp)
        {
            RedisCommandView view = new RedisCommandView((RedisCommandApp) paneApp);
            ((RedisCommandApp) paneApp).inject(view);

            return view;
        }
    };
}

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
package com.mymanyapps.mma.model;

import com.mymanyapps.mma.app.AppStatusApp;
import com.mymanyapps.mma.app.CoreApp;
import com.mymanyapps.mma.app.EmptyPaneApp;
import com.mymanyapps.mma.app.PaneAppFactory;
import com.mymanyapps.mma.app.ToCPaneApp;
import com.mymanyapps.mma.model.paneapp.AbstractPaneApp;
import com.mymanyapps.mma.view.AbstractAppView;
import com.mymanyapps.mma.view.AppStatusAppView;
import com.mymanyapps.mma.view.NoView;
import com.mymanyapps.mma.view.PaneViewFactory;
import com.mymanyapps.mma.view.ToCPaneAppView;

public enum PaneAppType
    implements PaneAppFactory, PaneViewFactory
{
    APP_STATUS
    {
        @Override
        public AbstractPaneApp<?> buildApp()
        {
            return new AppStatusApp();
        }

        @Override
        public AbstractAppView<?> buildView(AbstractPaneApp<?> paneApp)
        {
            AppStatusAppView view = new AppStatusAppView((AppStatusApp) paneApp);
            ((AppStatusApp) paneApp).inject(view);

            return view;
        }
    },
    CORE
    {
        @Override
        public AbstractPaneApp<?> buildApp()
        {
            return new CoreApp();
        }

        @Override
        public AbstractAppView<?> buildView(AbstractPaneApp<?> paneApp)
        {
            NoView view = new NoView((CoreApp) paneApp);
            ((CoreApp) paneApp).inject(view);

            return view;
        }
    },
    TOC
    {
        @Override
        public AbstractPaneApp<?> buildApp()
        {
            return new ToCPaneApp();
        }

        @Override
        public AbstractAppView<?> buildView(AbstractPaneApp<?> paneApp)
        {
            ToCPaneAppView view = new ToCPaneAppView((ToCPaneApp) paneApp);
            ((ToCPaneApp) paneApp).inject(view);

            return view;
        }
    },
    EMPTY
    {
        @Override
        public AbstractPaneApp<?> buildApp()
        {
            return new EmptyPaneApp();
        }

        @Override
        public AbstractAppView<?> buildView(AbstractPaneApp<?> paneApp)
        {
            return null;
        }
    };
}

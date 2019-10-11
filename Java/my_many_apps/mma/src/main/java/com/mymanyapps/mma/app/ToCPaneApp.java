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
package com.mymanyapps.mma.app;

import com.mymanyapps.mma.model.AbstractPaneModel;
import com.mymanyapps.mma.model.paneapp.AbstractPaneApp;
import com.mymanyapps.mma.view.ToCPaneAppView;

public class ToCPaneApp
    extends AbstractPaneApp<ToCPaneAppView>
{
    private ToCPaneAppView view;

    public ToCPaneApp()
    {
        super();
    }

    public void add(int x, int y, AbstractPaneModel model)
    {
        view.add(x, y, model);
    }

    @Override
    public void inject(ToCPaneAppView view)
    {
        this.view = view;
    }

    public void gotoPanel(AbstractPaneModel paneModel)
    {
        CoreApp coreApp = registrar.retrieveCoreApp();
        coreApp.showPanel(paneModel);
    }

    public int getSize()
    {
        return registrar.retrieveCoreApp().getSize();
    }
}

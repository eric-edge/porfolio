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
package com.mymanyapps.mma.view;

import com.mymanyapps.mma.model.AbstractPaneModel;
import com.mymanyapps.mma.model.CoreModel;
import com.mymanyapps.mma.model.PaneModel;
import com.mymanyapps.mma.model.paneapp.AbstractPaneApp;
import com.mymanyapps.mma.presenter.Presenter;

import javafx.scene.Scene;

public class View
    extends AbstractSceneView
{
    private final int size;
    private final Presenter presenter;
    private final CoreModel model;

    private Pane delegate;
    private Scene scene;

    public View(int size)
    {
        this.size = size;
        presenter = new Presenter(this);
        model = presenter.getModel();
    }

    public Presenter getPresenter()
    {
        return presenter;
    }

    public int getSize()
    {
        return size;
    }

    @Override
    public Scene getScene()
    {
        if (scene == null)
        {
            this.delegate = buildContentPane();
            this.scene = new Scene(delegate);
        }

        return scene;
    }

    private Pane buildContentPane()
    {
        delegate = new Pane(model, this, size);

        return delegate;
    }

    public void initCorePane(int y, PaneModel paneModel, AbstractPaneApp<?> coreApp)
    {
        delegate.addCorePane(y, paneModel, coreApp);
    }

    public void initUserPane(int x, int y, AbstractPaneModel paneModel, AbstractPaneApp<?> paneApp)
    {
        delegate.addUserPane(x, y, paneModel, paneApp);
    }

    public void init()
    {
        delegate.build();
    }

    public void showPanel(AbstractPaneModel paneModel)
    {
        delegate.showPanel(paneModel);
    }

    public void quit()
    {
        presenter.quit();
    }
}

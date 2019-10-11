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
package com.mymanyapps.mma.presenter;

import com.google.common.base.Preconditions;
import com.mymanyapps.mma.app.AppId;
import com.mymanyapps.mma.app.AppStatusApp;
import com.mymanyapps.mma.app.CoreApp;
import com.mymanyapps.mma.app.ToCPaneApp;
import com.mymanyapps.mma.model.AbstractModel;
import com.mymanyapps.mma.model.AbstractPaneModel;
import com.mymanyapps.mma.model.CoreModel;
import com.mymanyapps.mma.model.PaneModel;
import com.mymanyapps.mma.model.paneapp.AbstractPaneApp;
import com.mymanyapps.mma.view.View;

import javafx.application.Platform;

public class Presenter
    extends AbstractPresenter<View>
{
    private final CoreModel model;
    private final CoreApp coreApp;

    private AbstractModel userModel;

    public Presenter(View view)
    {
        super(view);

        this.model = new CoreModel();
        this.coreApp = (CoreApp) model.get(0, 1).buildApp();
        this.coreApp.setSize(view.getSize());
        this.coreApp.inject(this);
    }

    public void init()
    {
        AppId.Category category;

        // build Core apps
        category = AppId.Category.CORE;
        for (int y = 0; y < CoreModel.CORE_SIZE; y++)
        {
            PaneModel corePaneModel = model.get(0, y);
            AbstractPaneApp<?> corePaneApp = corePaneModel.buildApp();
            corePaneApp.inject(coreApp);
            AppId coreAppId = new AppId(category, 0, y, corePaneModel.getAppName());
            coreApp.register(coreAppId, corePaneApp);
            view.initCorePane(y, corePaneModel, corePaneApp);
        }

        // build user apps
        AppStatusApp statusApp = (AppStatusApp) coreApp.retrieveStatusApp();
        Preconditions.checkNotNull(statusApp);

        if (userModel != null)
        {
            category = AppId.Category.USER;
            for (int x = 0; x < userModel.getSize(); x++)
            {
                for (int y = 0; y < userModel.getSize(); y++)
                {
                    AbstractPaneModel paneModel = userModel.get(x, y);
                    AbstractPaneApp<?> paneApp = paneModel.buildApp();
                    paneApp.inject(coreApp);
                    AppId appId = new AppId(category, x, y, paneModel.getAppName());
                    coreApp.register(appId, paneApp);
                    view.initUserPane(x, y, paneModel, paneApp);
                    statusApp.addObservedApp(paneApp.getStatus());
                }
            }
        }

        // initialize the view
        view.init();

        // build ToA
        ToCPaneApp tocPaneApp = (ToCPaneApp) coreApp.retrieveToA();
        Preconditions.checkNotNull(tocPaneApp);

        // init ToA
        for (int x = 0; x < userModel.getSize(); x++)
        {
            for (int y = 0; y < userModel.getSize(); y++)
            {
                AbstractPaneModel paneModel = userModel.get(x, y);
                tocPaneApp.add(x, y, paneModel);
            }
        }

        // start the apps
        coreApp.start();
    }

    public void setUserModel(AbstractModel userModel)
    {
        this.userModel = userModel;
    }

    public AbstractPaneApp<?> getPaneApp(String name)
    {
        return coreApp.retrieve(null);
    }

    public CoreModel getModel()
    {
        return model;
    }

    public View getView()
    {
        return view;
    }

    public boolean hasNextId()
    {
        return model.hasNext();
    }

    public Long nextId()
    {
        return model.next();
    }

    public void showPanel(AbstractPaneModel paneModel)
    {
        view.showPanel(paneModel);
    }

    public void quit()
    {
        coreApp.stop();
        Platform.exit();
    }
}

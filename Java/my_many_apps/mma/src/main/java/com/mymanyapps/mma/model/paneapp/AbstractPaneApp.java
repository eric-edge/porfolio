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
package com.mymanyapps.mma.model.paneapp;

import com.mymanyapps.mma.app.AbstractAppStatus;
import com.mymanyapps.mma.app.AbstractAppStatus.OperationalStatus;
import com.mymanyapps.mma.app.AbstractAppStatus.Status;
import com.mymanyapps.mma.app.AppRegistrar;
import com.mymanyapps.mma.app.AppStatusProvider;
import com.mymanyapps.mma.model.AbstractPaneModel;
import com.mymanyapps.mma.view.AbstractAppView;

import javafx.scene.control.Control;
import javafx.scene.control.Label;

public abstract class AbstractPaneApp<V extends AbstractAppView<?>>
    implements AppStatusProvider
{
    protected AppRegistrar registrar;
    private AbstractPaneModel model;
    protected V view;

    protected AbstractAppStatus appStatus;
    protected AbstractAppStatus.Status status = null;
    protected AbstractAppStatus.OperationalStatus operationalStatus = null;

    protected AbstractPaneApp()
    {
        initAppStatus();
    }

    protected void initAppStatus()
    {
        appStatus = new AbstractAppStatus()
        {
            @Override
            public String textual(Details detailLevel)
            {
                StringBuilder sb = new StringBuilder();
                sb.append(AbstractPaneApp.this.getClass().getSimpleName());
                sb.append(" : [");
                if (status == null)
                {
                    sb.append("n/a");
                }
                else
                {
                    sb.append(status.name());
                }
                sb.append("] ");

                if (operationalStatus != null)
                {
                    sb.append(operationalStatus.name());
                }

                return sb.toString();
            }

            @Override
            public Control graphical()
            {
                Label statusLabel = new Label();
                statusLabel.setText(textual(null));

                return statusLabel;
            }
        };
    }

    public void inject(AppRegistrar registrar)
    {
        this.registrar = registrar;
    }

    public AppRegistrar getRegistrar()
    {
        return registrar;
    }

    @Override
    public AbstractAppStatus getStatus()
    {
        return appStatus;
    }

    public void init(AbstractPaneModel model)
    {
        status = Status.GREEN;
        operationalStatus = OperationalStatus.INIT;
        appStatus.setChanged();
        appStatus.notifyObservers();

        this.model = model;
    }

    public void start()
    {
        status = Status.GREEN;
        operationalStatus = OperationalStatus.RUNNING;
        appStatus.setChanged();
        appStatus.notifyObservers();
    }

    public void stop()
    {
        status = Status.GREEN;
        operationalStatus = OperationalStatus.EXITING;
        appStatus.setChanged();
        appStatus.notifyObservers();
    }

    abstract public void inject(V view);
}

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

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import com.mymanyapps.mma.app.AbstractAppStatus.OperationalStatus;
import com.mymanyapps.mma.app.AbstractAppStatus.Status;
import com.mymanyapps.mma.app.BaseAppId.Category;
import com.mymanyapps.mma.model.AbstractPaneModel;
import com.mymanyapps.mma.model.paneapp.AbstractPaneApp;
import com.mymanyapps.mma.presenter.Presenter;
import com.mymanyapps.mma.view.AbstractAppView;
import com.mymanyapps.mma.view.NoView;

public class CoreApp
    extends AbstractPaneApp<NoView>
    implements AppRegistrar
{
    private final Map<AppId, AbstractPaneApp<?>> register;

    private CoreAppStatus coreAppStatus;
    private Presenter presenter;

    private int size;

    public CoreApp()
    {
        super();

        this.register = new HashMap<>();
        this.size = 0;
    }

    @Override
    public AbstractPaneApp<?> retrieve(AppId appId)
    {
        if (appId == null)
        {
            return null;
        }

        return register.get(appId);
    }

    @Override
    public Collection<AppId> find(BaseAppId appId)
    {
        List<AppId> apps =
            register.entrySet().stream()
                .filter(e -> e.getKey().match(appId))
                .map(e -> e.getKey())
                .collect(Collectors.toList());

        return apps;
    }

    @Override
    public boolean isAvailable(AppId appId)
    {
        // TODO manage reservation
        return true;
    }

    @Override
    public void inject(NoView view)
    {
        ;
    }

    @Override
    public <V extends AbstractAppView<?>> void register(
        AppId appId,
        AbstractPaneApp<V> app)
    {
        register.put(appId, app);
    }

    @Override
    public AbstractAppStatus getStatus()
    {
        if (coreAppStatus == null)
        {
            coreAppStatus = new CoreAppStatus(this);
        }

        return coreAppStatus;
    }

    @Override
    public void start()
    {
        register.values().stream()
            .forEach(a -> a.start());

        status = Status.GREEN;
        operationalStatus = OperationalStatus.RUNNING;
        appStatus.setChanged();
        appStatus.notifyObservers();
    }

    @Override
    public void stop()
    {
        super.stop();

        register.values().stream()
            .forEach(a -> a.stop());
    }

    @Override
    public CoreApp retrieveCoreApp()
    {
        return this;
    }

    @Override
    public AbstractPaneApp<?> retrieveToA()
    {
        BaseAppId appId = new BaseAppId(Category.CORE, 0, 0, null);
        Collection<AppId> appIds = find(appId);
        if (appIds.isEmpty())
        {
            return null;
        }

        return retrieve(appIds.iterator().next());
    }

    @Override
    public AbstractPaneApp<?> retrieveStatusApp()
    {
        BaseAppId appId = new BaseAppId(Category.CORE, 0, 2, null);
        Collection<AppId> appIds = find(appId);
        if (appIds.isEmpty())
        {
            return null;
        }

        return retrieve(appIds.iterator().next());
    }

    public void showPanel(AbstractPaneModel paneModel)
    {
        if (presenter == null)
        {
            return;
        }

        presenter.showPanel(paneModel);
    }

    public void inject(Presenter presenter)
    {
        this.presenter = presenter;
    }

    public void setSize(int size)
    {
        this.size = size;
    }

    public int getSize()
    {
        return size;
    }
}

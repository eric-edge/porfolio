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
package com.mymanyapps.mmd.app;

import java.time.LocalDateTime;
import java.util.Timer;
import java.util.TimerTask;

import com.mymanyapps.mma.model.paneapp.AbstractPaneApp;
import com.mymanyapps.mmd.model.PaneAppType;
import com.mymanyapps.mmd.view.ClockPaneAppView;

import javafx.application.Platform;

public class ClockPaneApp
    extends AbstractPaneApp<ClockPaneAppView>
{
    private boolean isRunning = false;

    @Override
    public void inject(ClockPaneAppView view)
    {
        this.view = view;
    }

    public void setDateTime(LocalDateTime dateTime)
    {
        view.setDateTime(dateTime);
    }

    public PaneAppType getType()
    {
        return PaneAppType.CLOCK;
    }

    @Override
    public void stop()
    {
        super.stop();

        isRunning = false;
    }

    @Override
    public void start()
    {
        isRunning = true;

        Timer timer = new Timer();
        timer.scheduleAtFixedRate(new TimerTask()
        {
            @Override
            public void run()
            {
                if (!isRunning)
                {
                    timer.cancel();
                }
                else
                {
                    Platform.runLater(() -> view.setDateTime(LocalDateTime.now()));
                }
            }
        },
        1000,
        1000);

        super.start();
    }
}

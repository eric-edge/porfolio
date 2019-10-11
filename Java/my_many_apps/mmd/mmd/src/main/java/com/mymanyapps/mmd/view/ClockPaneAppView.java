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
package com.mymanyapps.mmd.view;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

import com.mymanyapps.mma.view.AbstractAppView;
import com.mymanyapps.mmd.app.ClockPaneApp;

import javafx.scene.control.Label;

public class ClockPaneAppView
    extends AbstractAppView<ClockPaneApp>
{
    private final Label label;
    private LocalDateTime dateTime;

    public ClockPaneAppView(ClockPaneApp paneApp)
    {
        this(paneApp, LocalDateTime.now());
    }

    public ClockPaneAppView(ClockPaneApp paneApp, LocalDateTime dateTime)
    {
        super(paneApp);

        this.dateTime = dateTime;
        this.label = new Label();
        setClock(dateTime);

        initComponents();
    }

    @Override
    protected void initComponents()
    {
        super.initComponents();

        setCenter(label);
    }

    public void setDateTime(LocalDateTime dateTime)
    {
        this.dateTime = dateTime;
        setClock(dateTime);
    }

    public LocalDateTime getClock()
    {
        return dateTime;
    }

    private void setClock(LocalDateTime dateTime)
    {
        label.setText(dateTime.format(DateTimeFormatter.ofPattern("HH:mm:ss")));
    }
}

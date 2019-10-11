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

import javafx.scene.control.Control;
import javafx.scene.control.Label;

public class CoreAppStatus
    extends AbstractAppStatus
{
    private final CoreApp coreApp;

    public CoreAppStatus(CoreApp coreApp)
    {
        this.coreApp = coreApp;
    }
    @Override
    public String textual(Details detailLevel)
    {
        return  getStatus();
    }

    @Override
    public Control graphical()
    {
        Label statusLabel = new Label();
        statusLabel.setText(textual(null));

        return statusLabel;
    }

    private String getStatus()
    {
        StringBuilder sb = new StringBuilder();
        if (coreApp == null)
        {
            sb.append("CoreApp : " + Status.RED.name());
        }
        else
        {
            sb.append(coreApp.getClass().getName());
            sb.append(" : ");
            sb.append(Status.GREEN.name());
        }

        return sb.toString();
    }
}

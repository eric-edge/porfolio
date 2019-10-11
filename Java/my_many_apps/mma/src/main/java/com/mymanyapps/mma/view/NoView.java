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

import com.mymanyapps.mma.app.CoreApp;
import com.mymanyapps.mma.model.paneapp.AbstractPaneApp;

import javafx.scene.control.Label;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;

public class NoView
    extends AbstractAppView<AbstractPaneApp<?>>
{
    private final Label label;

    public NoView(AbstractPaneApp<?> paneApp)
    {
        super(paneApp);

        this.label = new Label();
        Image image = new Image(CoreApp.class.getResourceAsStream("mma256.png"));
        label.setGraphic(new ImageView(image));

        setCenter(label);
    }
}

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
package com.mymanyapps.mmd.main;

import com.mymanyapps.mma.view.View;
import com.mymanyapps.mmd.model.Model;

import javafx.application.Application;
import javafx.scene.image.Image;
import javafx.stage.Stage;

public final class Main
    extends Application
{
//    protected static final Logger parentLogger = LogManager.getLogger();
//
//    private Logger logger = parentLogger;
//
//    protected Logger getLogger()
//    {
//        return logger;
//    }
//
//    protected void setLogger(Logger logger)
//    {
//        this.logger = logger;
//    }
//
//    public void log(Marker marker)
//    {
//        logger.debug(marker, "Parent log message");
//    }

    @Override
    public void start(Stage primaryStage) throws Exception
    {
        primaryStage.setTitle("My Many Demos");
        primaryStage.getIcons().add(new Image(Main.class.getResourceAsStream("mma.png")));

        Model model = new Model();
        View view = new View(model.getSize());
        view.getPresenter().setUserModel(model);
        primaryStage.setScene(view.getScene());
        primaryStage.setMaximized(true);
        view.getPresenter().init();

        primaryStage.setOnCloseRequest(e -> view.getPresenter().quit());

//        log(MarkerManager.getMarker("Showing stage"));
        primaryStage.show();
    }

    @Override
    public void stop() throws Exception
    {
        super.stop();
    }

    public static void main(String[] args)
    {
        Application.launch(Main.class, args);
    }
}

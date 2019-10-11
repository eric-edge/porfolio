package com.mymanyapps.mmd.presenter;

import com.mymanyapps.mmd.app.RedisCommandApp;

public class RedisCommandPresenter
{
    private final RedisCommandApp paneApp;

    public RedisCommandPresenter(RedisCommandApp paneApp)
    {
        this.paneApp = paneApp;

        paneApp.init();

        post("s", "K7", "V7");
    }

    public void post(String stream, String key, String msg)
    {
        paneApp.post(stream, key, msg);
    }
}

package com.mymanyapps.mmd.app;

import java.util.HashMap;
import java.util.Map;

import com.mymanyapps.mma.model.paneapp.AbstractPaneApp;
import com.mymanyapps.mmd.view.RedisCommandView;

import io.lettuce.core.RedisClient;
import io.lettuce.core.XAddArgs;
import io.lettuce.core.api.StatefulRedisConnection;
import io.lettuce.core.api.sync.RedisCommands;
import io.lettuce.core.codec.Utf8StringCodec;
import io.lettuce.core.protocol.CommandArgs;

public class RedisCommandApp
    extends AbstractPaneApp<RedisCommandView>
{
    private final String SERVER = "redis://192.168.0.197:6379/0";

    private RedisClient redisClient = RedisClient.create();
    private StatefulRedisConnection<String, String> connection;
    private RedisCommands<String, String> syncCommands;

    @Override
    public void inject(RedisCommandView view)
    {
        this.view = view;
    }

    public void init()
    {
        redisClient = RedisClient.create(SERVER);
        connection = redisClient.connect();
        syncCommands = connection.sync();
    }

    public void post(String stream, String key, String msg)
    {
        Map<String, String> map = new HashMap<>();
        map.put(key, msg);
        CommandArgs<String, String> commandArgs = new CommandArgs<>(Utf8StringCodec.UTF8);
//        commandArgs.add(map);
        XAddArgs args = new XAddArgs().id("*");
        args.build(commandArgs);

        syncCommands.xadd(stream, args, map);
    }

    @Override
    public void stop()
    {
        super.stop();

        connection.close();
        redisClient.shutdown();
    }
}

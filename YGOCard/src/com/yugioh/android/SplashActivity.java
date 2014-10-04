package com.yugioh.android;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.KeyEvent;
import com.rarnu.utils.ResourceUtils;
import com.rarnu.utils.UIUtils;

import java.util.Timer;
import java.util.TimerTask;

public class SplashActivity extends Activity {

    private Timer tmr;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_splash);
        UIUtils.initDisplayMetrics(this, getWindowManager(), false);
        ResourceUtils.init(this);
        tmr = new Timer();
        tmr.schedule(new TimerTask() {
            @Override
            public void run() {
                tmr.cancel();
                startMainActivity();
                SplashActivity.this.finish();
            }
        }, 1500);
    }

    private void startMainActivity() {
        Intent inMain = new Intent(this, MainActivity.class);
        startActivity(inMain);
    }

    @Override
    public boolean onKeyDown(int keyCode, KeyEvent event) {
        if (keyCode == KeyEvent.KEYCODE_BACK) {
            return true;
        }
        return super.onKeyDown(keyCode, event);
    }
}

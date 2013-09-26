package com.rarnu.tools.root;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.os.Bundle;
import android.view.KeyEvent;
import android.widget.TextView;
import com.rarnu.command.RootUtils;
import com.rarnu.tools.root.utils.DeviceUtils;
import com.rarnu.tools.root.utils.DirHelper;
import com.rarnu.utils.MiscUtils;
import com.rarnu.utils.UIUtils;

import java.util.Timer;
import java.util.TimerTask;

public class SplashActivity extends Activity {

    TextView tvVersion;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        UIUtils.initDisplayMetrics(this, getWindowManager(), true);
        super.onCreate(savedInstanceState);

        RootUtils.init(this);
        GlobalInstance.init(this);

        if (!MiscUtils.isSDCardExists()) {

            new AlertDialog.Builder(this)
                    .setTitle(R.string.hint)
                    .setMessage(R.string.no_sdcard_found)
                    .setPositiveButton(R.string.ok, new DialogInterface.OnClickListener() {

                        @Override
                        public void onClick(DialogInterface dialog, int which) {
                            finish();

                        }
                    })
                    .show();

            return;
        }

        setContentView(R.layout.layout_splash);
        tvVersion = (TextView) findViewById(R.id.tvVersion);

        String ver = getString(R.string.app_ver, DeviceUtils.getAppVersionName(this));
        if (GlobalInstance.DEBUG) {
            ver += getString(R.string.app_debug);
        }

        tvVersion.setText(ver);

        DirHelper.makeDir();

        final Timer tmrClose = new Timer();
        tmrClose.schedule(new TimerTask() {

            @Override
            public void run() {
                tmrClose.cancel();
                finish();
                startMainActivity();
            }
        }, 2000);

    }

    private void startMainActivity() {
        Intent inMain = new Intent(this, MainActivity.class);
        inMain.setFlags(Intent.FLAG_ACTIVITY_NO_USER_ACTION);
        startActivity(inMain);
    }

    @Override
    public boolean onKeyDown(int keyCode, KeyEvent event) {
        return true;
    }

}

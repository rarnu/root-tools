package com.rarnu.tools.root;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.Loader;
import android.graphics.BitmapFactory;
import android.os.Bundle;
import android.view.KeyEvent;
import android.widget.ImageView;
import android.widget.TextView;
import com.rarnu.command.RootUtils;
import com.rarnu.tools.root.api.MobileApi;
import com.rarnu.tools.root.loader.SplashLoader;
import com.rarnu.tools.root.utils.DeviceUtils;
import com.rarnu.tools.root.utils.DirHelper;
import com.rarnu.tools.root.utils.IptablesUtils;
import com.rarnu.tools.root.utils.ShareUtils;
import com.rarnu.utils.DownloadUtils;
import com.rarnu.utils.MiscUtils;
import com.rarnu.utils.ResourceUtils;
import com.rarnu.utils.UIUtils;

import java.io.File;
import java.util.Timer;
import java.util.TimerTask;

public class SplashActivity extends Activity implements Loader.OnLoadCompleteListener<String> {

    private static final String SPLASH_FILENAME = "splash";
    TextView tvVersion;
    ImageView imgSplash;
    SplashLoader loader;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        init();
        super.onCreate(savedInstanceState);

        if (!checkSDCard()) {
            return;
        }

        setContentView(R.layout.layout_splash);
        tvVersion = (TextView) findViewById(R.id.tvVersion);
        imgSplash = (ImageView) findViewById(R.id.imgSplash);
        loader = new SplashLoader(this);
        loader.registerListener(0, this);
        loader.startLoading();
        showVersion();
        loadLocalSplashImage();

        DirHelper.makeDir();
        ShareUtils.registerToWechat(this);
        startCloseTimer();
    }

    private void init() {
        UIUtils.initDisplayMetrics(this, getWindowManager(), true);
        ResourceUtils.init(this);
        setTheme(GlobalInstance.theme ? android.R.style.Theme_Holo_Light_NoActionBar_Fullscreen : android.R.style.Theme_Holo_NoActionBar_Fullscreen);
        RootUtils.init(this);
        GlobalInstance.init(this);
        IptablesUtils.init();

    }

    private boolean checkSDCard() {
        boolean ret = MiscUtils.isSDCardExists();
        if (!ret) {

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
        }
        return ret;
    }

    private void showVersion() {
        String ver = getString(R.string.app_ver, DeviceUtils.getAppVersionName(this));
        if (GlobalInstance.DEBUG) {
            ver += getString(R.string.app_debug);
        }

        tvVersion.setText(ver);

    }

    private void startCloseTimer() {
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

    private void loadLocalSplashImage() {
        String fileName = DirHelper.TEMP_DIR + SPLASH_FILENAME;
        if (new File(fileName).exists()) {
            try {
                imgSplash.setImageBitmap(BitmapFactory.decodeFile(fileName));
            } catch (Exception e) {

            }
        }
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

    @Override
    public void onLoadComplete(Loader<String> loader, String data) {
        try {
            DownloadUtils.downloadFileT(this, imgSplash, MobileApi.SPLASH_BASE_URL + data, DirHelper.TEMP_DIR, SPLASH_FILENAME, null);
        } catch (Exception e) {

        }
    }
}

package com.rarnu.tools.root;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.Loader;
import android.graphics.BitmapFactory;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.KeyEvent;
import android.widget.ImageView;
import android.widget.TextView;
import com.rarnu.command.RootUtils;
import com.rarnu.tools.root.api.MobileApi;
import com.rarnu.tools.root.loader.SplashLoader;
import com.rarnu.tools.root.utils.*;
import com.rarnu.utils.*;
import com.rarnu.utils.FileUtils;

import java.io.File;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;

public class SplashActivity extends Activity implements Loader.OnLoadCompleteListener<String> {

    private static final String SPLASH_FILENAME = "splash";
    TextView tvVersion;
    ImageView imgSplash;
    SplashLoader loader;
    TextView tvOfficialCheck;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        init();
        super.onCreate(savedInstanceState);

        if (checkEmulator()) {
            return;
        }

        if (!checkSDCard()) {
            return;
        }

        setContentView(R.layout.layout_splash);
        tvVersion = (TextView) findViewById(R.id.tvVersion);
        imgSplash = (ImageView) findViewById(R.id.imgSplash);
        tvOfficialCheck = (TextView) findViewById(R.id.tvOfficialCheck);
        loader = new SplashLoader(this);
        loader.registerListener(0, this);
        loader.startLoading();
        showVersion();
        checkSignatureT();
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

    private boolean checkEmulator() {
        boolean ret = MiscUtils.isEmulator(this);
        if (ret) {
            new AlertDialog.Builder(this)
                    .setTitle(R.string.hint)
                    .setMessage(R.string.emulator_hint)
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
            FileUtils.deleteFile(DirHelper.TEMP_DIR + SPLASH_FILENAME);
            DownloadUtils.downloadFileT(this, imgSplash, MobileApi.SPLASH_BASE_URL + data, DirHelper.TEMP_DIR, SPLASH_FILENAME, null);
        } catch (Exception e) {

        }
    }

    private Handler hSignature = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1) {
                tvOfficialCheck.setTextColor(getResources().getColor(GlobalInstance.isOfficialVersion ? R.color.greenyellow : R.color.red));
                tvOfficialCheck.setText(GlobalInstance.isOfficialVersion ? R.string.official_yes : R.string.official_no);
            }
            super.handleMessage(msg);
        }
    };

    private void checkSignatureT() {
        new Thread(new Runnable() {
            @Override
            public void run() {
                try {
                    String standardSig = FileUtils.readAssetFile(SplashActivity.this, "sig").trim();
                    String apkPath = getPackageManager().getApplicationInfo(getPackageName(), 0).publicSourceDir;
                    List<String> sigs = SignatureUtils.getSignaturesFromApk(apkPath);
                    String sig = sigs.get(0).trim();
                    String finalSig = sig.substring(0, 15) + sig.substring(sig.length() - 15);
                    GlobalInstance.isOfficialVersion = finalSig.equals(standardSig);
                } catch (Exception e) {
                    GlobalInstance.isOfficialVersion = false;
                }
                hSignature.sendEmptyMessage(1);
            }
        }).start();
    }
}

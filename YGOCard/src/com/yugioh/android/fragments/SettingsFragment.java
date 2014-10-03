package com.yugioh.android.fragments;

import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.*;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.FileUtils;
import com.yugioh.android.R;
import com.yugioh.android.common.Config;
import com.yugioh.android.define.NetworkDefine;
import com.yugioh.android.define.PathDefine;

import java.io.File;

public class SettingsFragment extends BaseFragment implements OnClickListener {

    ImageView btnBigger, btnSmaller;
    TextView tvFontDemo;
    TextView tvData;
    Button btnSource;

    int fontSize = -1;

    @Override
    public int getBarTitle() {
        return R.string.settings;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.settings;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public void initComponents() {
        btnBigger = (ImageButton) innerView.findViewById(R.id.btnBigger);
        btnSmaller = (ImageButton) innerView.findViewById(R.id.btnSmaller);
        tvData = (TextView) innerView.findViewById(R.id.tvData);
        tvFontDemo = (TextView) innerView.findViewById(R.id.tvFontDemo);
        btnSource = (Button) innerView.findViewById(R.id.btnSource);
    }

    @Override
    public void initEvents() {
        btnBigger.setOnClickListener(this);
        btnSmaller.setOnClickListener(this);
        tvData.setOnClickListener(this);
        btnSource.setOnClickListener(this);
    }

    @Override
    public void initLogic() {
        fontSize = Config.cfgGetFontSize(getActivity());
        if (fontSize == -1) {
            fontSize = (int) tvFontDemo.getTextSize();
        }
        tvFontDemo.setTextSize(fontSize);
        getDirSizeT();
    }

    private void getDirSizeT() {
        final Handler hSize = new Handler() {
            @Override
            public void handleMessage(Message msg) {
                if (msg.what == 1) {
                    tvData.setText(String.format("%d MB", (Long) msg.obj));
                }
                super.handleMessage(msg);
            }
        };

        new Thread(new Runnable() {

            @Override
            public void run() {
                Long size = FileUtils.getDirSize(PathDefine.ROOT_PATH);
                size /= (1024 * 1024);
                Message msg = new Message();
                msg.what = 1;
                msg.obj = size;
                hSize.sendMessage(msg);
            }
        }).start();

    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.fragment_settings;
    }

    @Override
    public String getMainActivityName() {
        return "";
    }

    @Override
    public void initMenu(Menu menu) {

    }

    @Override
    public void onGetNewArguments(Bundle bn) {

    }

    @Override
    public void onClick(View v) {
        switch (v.getId()) {
            case R.id.btnBigger:
                fontSize++;
                break;
            case R.id.btnSmaller:
                fontSize--;
                break;
            case R.id.tvData:
                confirmDeleteImages();
                break;
            case R.id.btnSource:
                openSourceCodeSite();
                break;
        }
        tvFontDemo.setTextSize(fontSize);
        Config.cfgSetFontSize(getActivity(), fontSize);
    }

    private void openSourceCodeSite() {
        Intent inSite = new Intent(Intent.ACTION_VIEW);
        inSite.setData(Uri.parse(NetworkDefine.URL_GITHUB));
        startActivity(inSite);
    }

    private void confirmDeleteImages() {
        new AlertDialog.Builder(getActivity()).setTitle(R.string.hint).setMessage(R.string.str_confirm_delete)
                .setPositiveButton(R.string.ok, new DialogInterface.OnClickListener() {
                    @Override
                    public void onClick(DialogInterface dialogInterface, int i) {
                        doDeleteImageT();
                    }
                })
                .setNegativeButton(R.string.cancel, null)
                .show();
    }

    private Handler hDelete = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1) {
                getDirSizeT();
            }
            super.handleMessage(msg);
        }
    };

    private void doDeleteImageT() {
        new Thread(new Runnable() {
            @Override
            public void run() {
                File f = new File(PathDefine.PICTURE_PATH);
                for (String s: f.list()) {
                    new File(PathDefine.PICTURE_PATH+s).delete();
                }
                hDelete.sendEmptyMessage(1);
            }
        }).start();
    }

    @Override
    public Bundle getFragmentState() {
        return null;
    }
}

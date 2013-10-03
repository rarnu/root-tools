package com.yugioh.android.fragments;

import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.ImageView;
import android.widget.TextView;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.FileUtils;
import com.yugioh.android.R;
import com.yugioh.android.common.Config;
import com.yugioh.android.define.PathDefine;

public class SettingsFragment extends BaseFragment implements OnClickListener {

    // font settings

    private static final int[] fits = new int[]{R.drawable.c0, R.drawable.c1,
            R.drawable.c2, R.drawable.c3, R.drawable.c4, R.drawable.c5,
            R.drawable.c6, R.drawable.c7, R.drawable.c8, R.drawable.c9};
    ImageView ivFitable;
    Button btnBigger, btnSmaller;
    TextView tvFontDemo;
    TextView tvData;
    CheckBox chkAutoName;

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
        ivFitable = (ImageView) innerView.findViewById(R.id.ivFitable);
        btnBigger = (Button) innerView.findViewById(R.id.btnBigger);
        btnSmaller = (Button) innerView.findViewById(R.id.btnSmaller);
        tvData = (TextView) innerView.findViewById(R.id.tvData);
        tvFontDemo = (TextView) innerView.findViewById(R.id.tvFontDemo);
        chkAutoName = (CheckBox) innerView.findViewById(R.id.chkAutoName);
    }

    @Override
    public void initEvents() {
        btnBigger.setOnClickListener(this);
        btnSmaller.setOnClickListener(this);
        chkAutoName.setOnClickListener(this);
    }

    @Override
    public void initLogic() {
        ivFitable.setImageResource(fits[9]);

        fontSize = Config.cfgGetFontSize(getActivity());
        if (fontSize == -1) {
            fontSize = (int) tvFontDemo.getTextSize();
        }
        tvFontDemo.setTextSize(fontSize);
        getDirSizeT();
        chkAutoName.setChecked(Config.cfgGetAutoName(getActivity()));
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
            case R.id.chkAutoName:
                Config.cfgSetAutoName(getActivity(), chkAutoName.isChecked());
                return;
        }
        tvFontDemo.setTextSize(fontSize);
        Config.cfgSetFontSize(getActivity(), fontSize);
    }

    @Override
    public Bundle getFragmentState() {
        return null;
    }

}

package com.rarnu.miart;

import android.app.Activity;
import android.app.AlertDialog;
import android.os.Build;
import android.os.Bundle;
import android.text.Html;
import android.view.View;
import android.widget.Button;
import android.widget.TextView;
import com.rarnu.miart.command.RootUtils;

public class MainActivity extends Activity implements View.OnClickListener {

    TextView tvDesc;
    TextView tvEnv;
    Button btnSwitch;
    TextView tvSwitchResult;

    boolean isArt = false;

    @Override
    public void onCreate(Bundle savedInstanceState) {

        super.onCreate(savedInstanceState);
        setContentView(R.layout.main);

        RootUtils.init(this);

        // bind components
        tvDesc = (TextView) findViewById(R.id.tvDesc);
        tvEnv = (TextView) findViewById(R.id.tvEnv);
        btnSwitch = (Button) findViewById(R.id.btnSwitch);
        tvSwitchResult = (TextView) findViewById(R.id.tvSwitchResult);

        // bind events
        btnSwitch.setOnClickListener(this);

        // desc
        tvDesc.setText(Html.fromHtml(getString(R.string.app_desc)));

        // current vm
        isArt = VMLibUtils.isART();
        tvEnv.setText(getString(R.string.current_runtime, getString(isArt ? R.string.vm_art : R.string.vm_dalvik)));
        btnSwitch.setText(isArt ? R.string.btn_switch_to_dalvik : R.string.btn_switch_to_art);

        // check 4.4
        if (Build.VERSION.SDK_INT < 19) {
            btnSwitch.setEnabled(false);
            btnSwitch.setText(R.string.btn_cannot_switch);
        }
    }

    @Override
    public void onClick(View v) {
        switch (v.getId()) {
            case R.id.btnSwitch:
                doSwitchVM();
                break;
        }
    }

    private void doSwitchVM() {
        boolean switched = VMLibUtils.setArt(!isArt);
        if (!switched) {
            tvSwitchResult.setText(R.string.switch_failed);
        }
    }
}

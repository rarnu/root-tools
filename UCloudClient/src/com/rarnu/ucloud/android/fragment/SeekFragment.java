package com.rarnu.ucloud.android.fragment;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.widget.ImageView;
import android.widget.SeekBar;
import android.widget.TextView;
import com.rarnu.devlib.base.BaseDialogFragment;
import com.rarnu.ucloud.android.R;

public class SeekFragment extends BaseDialogFragment implements View.OnClickListener, SeekBar.OnSeekBarChangeListener {

    TextView tvTitle;
    ImageView btnSave, btnCancel;
    SeekBar sbPercent;
    TextView tvPercent;

    @Override
    public int getBarTitle() {
        return 0;
    }

    @Override
    public int getBarTitleWithPath() {
        return 0;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public void initComponents() {
        tvTitle = (TextView) innerView.findViewById(R.id.tvTitle);
        btnSave = (ImageView) innerView.findViewById(R.id.btnSave);
        btnCancel = (ImageView) innerView.findViewById(R.id.btnCancel);
        sbPercent = (SeekBar) innerView.findViewById(R.id.sbPercent);
        tvPercent = (TextView) innerView.findViewById(R.id.tvPercent);
    }

    @Override
    public void initEvents() {
        btnSave.setOnClickListener(this);
        btnCancel.setOnClickListener(this);
        sbPercent.setOnSeekBarChangeListener(this);
    }

    @Override
    public void initLogic() {
        tvTitle.setText(getArguments().getString("title"));
        sbPercent.setProgress(getArguments().getInt("progress"));
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.dialog_seek;
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
    public Bundle getFragmentState() {
        return null;
    }

    @Override
    public void onClick(View v) {
        switch (v.getId()) {
            case R.id.btnSave:
                Intent inRet = new Intent();
                inRet.putExtra("progress", sbPercent.getProgress());
                getActivity().setResult(Activity.RESULT_OK, inRet);
                getActivity().finish();
                break;
            case R.id.btnCancel:
                getActivity().finish();
                break;
        }
    }

    @Override
    public void onProgressChanged(SeekBar seekBar, int progress, boolean fromUser) {
        tvPercent.setText(String.format("%d%%", progress));
    }

    @Override
    public void onStartTrackingTouch(SeekBar seekBar) {
    }

    @Override
    public void onStopTrackingTouch(SeekBar seekBar) {
    }
}

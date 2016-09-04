package com.rarnu.tools.neo.fragment;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.TextView;
import com.rarnu.tools.neo.R;
import com.rarnu.tools.neo.base.BaseDialogFragment;
import com.rarnu.tools.neo.data.BuildPropInfo;

public class BuildPropEditFragment extends BaseDialogFragment implements View.OnClickListener {

    private TextView tvHead = null;
    private ImageView btnCancel = null;
    private ImageView btnSave = null;
    private EditText etValue = null;

    @Override
    public int getBarTitle() {
        return 0;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public void initComponents() {
        tvHead = (TextView) innerView.findViewById(R.id.tvHead);
        btnSave = (ImageView) innerView.findViewById(R.id.btnSave);
        btnCancel = (ImageView) innerView.findViewById(R.id.btnCancel);
        etValue = (EditText) innerView.findViewById(R.id.etValue);
    }

    @Override
    public void initEvents() {
        btnCancel.setOnClickListener(this);
        btnSave.setOnClickListener(this);
    }

    @Override
    public void initLogic() {
        BuildPropInfo item = (BuildPropInfo) getActivity().getIntent().getSerializableExtra("item");
        tvHead.setText(item.buildName);
        etValue.setText(item.buildValue);
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.fragment_fakedev_edit;
    }

    @Override
    public String getMainActivityName() {
        return null;
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
                BuildPropInfo info = new BuildPropInfo();
                info.buildName = tvHead.getText().toString();
                info.buildValue = etValue.getText().toString();
                Intent inRet = new Intent();
                inRet.putExtra("item", info);
                inRet.putExtra("position", getActivity().getIntent().getIntExtra("position", -1));
                getActivity().setResult(Activity.RESULT_OK, inRet);
                getActivity().finish();
                break;
            case R.id.btnCancel:
                getActivity().finish();
                break;
        }
    }
}

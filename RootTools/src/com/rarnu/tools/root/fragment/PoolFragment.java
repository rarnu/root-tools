package com.rarnu.tools.root.fragment;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.widget.AdapterView;
import android.widget.GridView;
import android.widget.ImageView;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.PoolAdapter;
import com.rarnu.tools.root.common.FileOperationInfo;

public class PoolFragment extends BaseFragment implements View.OnClickListener, AdapterView.OnItemClickListener {

    GridView gvPool;
    PoolAdapter adapter;
    ImageView btnClean;
    ImageView btnClose;

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
        btnClean = (ImageView) innerView.findViewById(R.id.btnClean);
        gvPool = (GridView) innerView.findViewById(R.id.gvPool);
        btnClose = (ImageView) innerView.findViewById(R.id.btnClose);
        adapter = new PoolAdapter(getActivity(), GlobalInstance.listOperation);
        gvPool.setAdapter(adapter);
    }

    @Override
    public void initEvents() {
        gvPool.setOnItemClickListener(this);
        btnClean.setOnClickListener(this);
        btnClose.setOnClickListener(this);
    }

    @Override
    public void initLogic() {
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_pool;
    }

    @Override
    public String getMainActivityName() {
        return MainActivity.class.getName();
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
            case R.id.btnClean:
                GlobalInstance.listOperation.clear();
                getActivity().setResult(Activity.RESULT_OK);
                getActivity().finish();
                break;
            case R.id.btnClose:
                getActivity().finish();
                break;
        }
    }

    @Override
    public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        FileOperationInfo item = (FileOperationInfo) gvPool.getItemAtPosition(position);
        Intent inRet = new Intent();
        inRet.putExtra("info", item);
        getActivity().setResult(Activity.RESULT_OK, inRet);
        getActivity().finish();
    }
}

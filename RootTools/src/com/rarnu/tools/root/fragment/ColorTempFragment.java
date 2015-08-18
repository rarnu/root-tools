package com.rarnu.tools.root.fragment;

import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.Button;
import android.widget.SeekBar;
import android.widget.TextView;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.DataProgressBar;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.tools.root.service.ScreenService;
import com.rarnu.utils.ImageUtils;

public class ColorTempFragment extends BaseFragment implements SeekBar.OnSeekBarChangeListener, View.OnClickListener {

    MenuItem itemEnabled;
    SeekBar sbRV, sbRA, sbGV, sbGA, sbBV, sbBA;
    TextView tvRV, tvRA, tvGV, tvGA, tvBV, tvBA;
    DataProgressBar progressCT;
    SharedPreferences pref;
    Button btnOneKeyOpt;
    private int r, g, b, rv, gv, bv;

    @Override
    public int getBarTitle() {
        return R.string.func_color_temp;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.func_color_temp_with_path;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public void initComponents() {
        sbRV = (SeekBar) innerView.findViewById(R.id.sbRV);
        sbRA = (SeekBar) innerView.findViewById(R.id.sbRA);
        sbGV = (SeekBar) innerView.findViewById(R.id.sbGV);
        sbGA = (SeekBar) innerView.findViewById(R.id.sbGA);
        sbBV = (SeekBar) innerView.findViewById(R.id.sbBV);
        sbBA = (SeekBar) innerView.findViewById(R.id.sbBA);
        tvRV = (TextView) innerView.findViewById(R.id.tvRV);
        tvRA = (TextView) innerView.findViewById(R.id.tvRA);
        tvGV = (TextView) innerView.findViewById(R.id.tvGV);
        tvGA = (TextView) innerView.findViewById(R.id.tvGA);
        tvBV = (TextView) innerView.findViewById(R.id.tvBV);
        tvBA = (TextView) innerView.findViewById(R.id.tvBA);
        progressCT = (DataProgressBar) innerView.findViewById(R.id.progressCT);
        btnOneKeyOpt = (Button) innerView.findViewById(R.id.btnOneKeyOpt);
    }

    @Override
    public void initEvents() {
        sbRV.setOnSeekBarChangeListener(this);
        sbRA.setOnSeekBarChangeListener(this);
        sbGV.setOnSeekBarChangeListener(this);
        sbGA.setOnSeekBarChangeListener(this);
        sbBV.setOnSeekBarChangeListener(this);
        sbBA.setOnSeekBarChangeListener(this);
        btnOneKeyOpt.setOnClickListener(this);
    }

    @Override
    public void initLogic() {
        progressCT.setAppName(getString(R.string.color_hint_install));

        pref = getActivity().getSharedPreferences("pref", 0);
        r = pref.getInt("r", 0);
        g = pref.getInt("g", 0);
        b = pref.getInt("b", 0);
        rv = pref.getInt("rv", 255);
        gv = pref.getInt("gv", 255);
        bv = pref.getInt("bv", 255);
        sbRV.setProgress(rv);
        sbRA.setProgress(r);
        sbGV.setProgress(gv);
        sbGA.setProgress(g);
        sbBV.setProgress(bv);
        sbBA.setProgress(b);
        tvRV.setText(String.valueOf(rv));
        tvGV.setText(String.valueOf(gv));
        tvBV.setText(String.valueOf(bv));
        tvRA.setText(String.valueOf(r));
        tvGA.setText(String.valueOf(g));
        tvBA.setText(String.valueOf(b));
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_color_temp;
    }

    @Override
    public String getMainActivityName() {
        return MainActivity.class.getName();
    }

    @Override
    public void initMenu(Menu menu) {
        itemEnabled = menu.add(0, MenuItemIds.MENU_ENABLED, 99, R.string.enable);
        itemEnabled.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
        itemEnabled.setIcon(ImageUtils.loadActionBarIcon(getActivity(), ScreenService.state ? R.drawable.firewall_enabled : R.drawable.firewall_disabled));
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case MenuItemIds.MENU_ENABLED:
                Intent inScreenService = new Intent(getActivity().getBaseContext(), ScreenService.class);
                if (ScreenService.state) {
                    getActivity().stopService(inScreenService);
                    itemEnabled.setIcon(ImageUtils.loadActionBarIcon(getActivity(), R.drawable.firewall_disabled));
                } else {
                    getActivity().startService(inScreenService);
                    itemEnabled.setIcon(ImageUtils.loadActionBarIcon(getActivity(), R.drawable.firewall_enabled));
                }

                break;
        }
        return true;
    }

    @Override
    public void onGetNewArguments(Bundle bn) {

    }

    @Override
    public Bundle getFragmentState() {
        return null;
    }

    @Override
    public void onProgressChanged(SeekBar seekBar, int progress, boolean fromUser) {
        switch (seekBar.getId()) {
            case R.id.sbRV:
                rv = seekBar.getProgress();
                tvRV.setText(String.valueOf(rv));
                break;
            case R.id.sbRA:
                r = seekBar.getProgress();
                tvRA.setText(String.valueOf(r));
                break;
            case R.id.sbGV:
                gv = seekBar.getProgress();
                tvGV.setText(String.valueOf(gv));
                break;
            case R.id.sbGA:
                g = seekBar.getProgress();
                tvGA.setText(String.valueOf(g));
                break;
            case R.id.sbBV:
                bv = seekBar.getProgress();
                tvBV.setText(String.valueOf(bv));
                break;
            case R.id.sbBA:
                b = seekBar.getProgress();
                tvBA.setText(String.valueOf(b));
                break;
        }
        save();
        ScreenService.setRGB(r, g, b, rv, gv, bv);
    }

    @Override
    public void onStartTrackingTouch(SeekBar seekBar) {

    }

    @Override
    public void onStopTrackingTouch(SeekBar seekBar) {

    }

    private void save() {
        SharedPreferences.Editor editor = pref.edit();
        editor.putInt("r", r);
        editor.putInt("g", g);
        editor.putInt("b", b);
        editor.putInt("rv", rv);
        editor.putInt("gv", gv);
        editor.putInt("bv", bv);
        editor.commit();
    }

    @Override
    public void onClick(View v) {
        switch (v.getId()) {
            case R.id.btnOneKeyOpt:
                sbRV.setProgress(255);
                sbRA.setProgress(0);
                sbGV.setProgress(255);
                sbGA.setProgress(0);
                sbBV.setProgress(100);
                sbBA.setProgress(15);
                break;
        }
    }
}

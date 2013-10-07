package com.rarnu.huawei.p6;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.graphics.Color;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.AdapterView;
import android.widget.Spinner;
import android.widget.TextView;
import android.widget.Toast;
import com.rarnu.command.RootUtils;
import com.rarnu.huawei.p6.adapter.ItemAdapter;
import com.rarnu.huawei.p6.common.FileDefine;
import com.rarnu.huawei.p6.common.MenuDefine;
import com.rarnu.huawei.p6.utils.Config;
import com.rarnu.huawei.p6.utils.CpuUtils;
import com.rarnu.huawei.p6.utils.DeviceCheckUtils;
import com.rarnu.utils.UIUtils;

public class MainActivity extends Activity implements AdapterView.OnItemSelectedListener {

    Spinner spCpuFreq, spCpuCore, spDdrFreq, spGpuFreq, spProfile;
    ItemAdapter adapterCpuFreq, adapterCpuCore, adapterDdrFreq, adapterGpuFreq, adapterProfile;
    String[] arrayCpuFreq, arrayCpuCore, arrayDdrFreq, arrayGpuFreq, arrayProfile;
    TextView tvApply;
    MenuItem itemSave, itemReset;
    int[] profiles = new int[]{R.array.profile_0, R.array.profile_1, R.array.profile_2, R.array.profile_3, R.array.profile_4, R.array.profile_5};

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        UIUtils.initDisplayMetrics(this, getWindowManager(), true);
        RootUtils.init(this);
        boolean isP6 = DeviceCheckUtils.isHuaweiP6();
        if (!isP6) {
            new AlertDialog.Builder(this)
                    .setTitle(R.string.hint)
                    .setMessage(R.string.not_huawei_p6)
                    .setPositiveButton(R.string.ok, new DialogInterface.OnClickListener() {
                        @Override
                        public void onClick(DialogInterface dialog, int which) {
                            finish();
                        }
                    })
                    .show();
            return;
        }


        setContentView(R.layout.main);

        initComponents();
        initEvents();
        initData();
        checkApplyState();
        RootUtils.mountRW();
    }

    private void initComponents() {
        spCpuFreq = (Spinner) findViewById(R.id.spCpuFreq);
        spCpuCore = (Spinner) findViewById(R.id.spCpuCore);
        spDdrFreq = (Spinner) findViewById(R.id.spDdrFreq);
        spGpuFreq = (Spinner) findViewById(R.id.spGpuFreq);
        spProfile = (Spinner) findViewById(R.id.spProfile);
        tvApply = (TextView) findViewById(R.id.tvApply);

        arrayCpuFreq = getResources().getStringArray(R.array.array_cpu_freq);
        arrayCpuCore = getResources().getStringArray(R.array.array_cpu_core);
        arrayDdrFreq = getResources().getStringArray(R.array.array_ddr_freq);
        arrayGpuFreq = getResources().getStringArray(R.array.array_gpu_freq);
        arrayProfile = getResources().getStringArray(R.array.array_profile);

        adapterCpuFreq = new ItemAdapter(this, arrayCpuFreq);
        adapterCpuCore = new ItemAdapter(this, arrayCpuCore);
        adapterDdrFreq = new ItemAdapter(this, arrayDdrFreq);
        adapterGpuFreq = new ItemAdapter(this, arrayGpuFreq);
        adapterProfile = new ItemAdapter(this, arrayProfile);

        spCpuFreq.setAdapter(adapterCpuFreq);
        spCpuCore.setAdapter(adapterCpuCore);
        spDdrFreq.setAdapter(adapterDdrFreq);
        spGpuFreq.setAdapter(adapterGpuFreq);
        spProfile.setAdapter(adapterProfile);
    }

    private void initEvents() {
        spProfile.setOnItemSelectedListener(this);
        spCpuFreq.setOnItemSelectedListener(this);
        spCpuCore.setOnItemSelectedListener(this);
        spDdrFreq.setOnItemSelectedListener(this);
        spGpuFreq.setOnItemSelectedListener(this);
    }

    private void initData() {
        spProfile.setSelection(Config.getProfile(this));
        spCpuFreq.setSelection(CpuUtils.getStringIndex(arrayCpuFreq, Config.getCpuFreq(this)));
        spCpuCore.setSelection(CpuUtils.getStringIndex(arrayCpuCore, Config.getCpuCore(this)));
        spDdrFreq.setSelection(CpuUtils.getStringIndex(arrayDdrFreq, Config.getDdrFreq(this)));
        spGpuFreq.setSelection(CpuUtils.getStringIndex(arrayGpuFreq, Config.getGpuFreq(this)));
    }

    private void saveData() {
        String cpuFreq = CpuUtils.getIndexedString(arrayCpuFreq, spCpuFreq.getSelectedItemPosition());
        String cpuCore = CpuUtils.getIndexedString(arrayCpuCore, spCpuCore.getSelectedItemPosition());
        String ddrFreq = CpuUtils.getIndexedString(arrayDdrFreq, spDdrFreq.getSelectedItemPosition());
        String gpuFreq = CpuUtils.getIndexedString(arrayGpuFreq, spGpuFreq.getSelectedItemPosition());
        boolean ret1 = CpuUtils.setIntValue(FileDefine.CPU_FREQ, cpuFreq);
        boolean ret2 = CpuUtils.setIntValue(FileDefine.CPU_CORE, cpuCore);
        boolean ret3 = CpuUtils.setIntValue(FileDefine.DDR_FREQ, ddrFreq);
        boolean ret4 = CpuUtils.setIntValue(FileDefine.GPU_FREQ, gpuFreq);
        boolean succ = (ret1 && ret2 && ret3 && ret4);
        Toast.makeText(this, succ ? R.string.save_succ : R.string.save_fail, Toast.LENGTH_LONG).show();

        Config.setProfile(this, spProfile.getSelectedItemPosition());
        Config.setCpuFreq(this, arrayCpuFreq[spCpuFreq.getSelectedItemPosition()]);
        Config.setCpuCore(this, arrayCpuCore[spCpuCore.getSelectedItemPosition()]);
        Config.setDdrFreq(this, arrayDdrFreq[spDdrFreq.getSelectedItemPosition()]);
        Config.setGpuFreq(this, arrayGpuFreq[spGpuFreq.getSelectedItemPosition()]);

        checkApplyState();

    }

    private void checkApplyState() {
        // check apply state
        boolean ret1 = arrayCpuFreq[spCpuFreq.getSelectedItemPosition()].equals(CpuUtils.getIntValue(FileDefine.CPU_FREQ));
        boolean ret2 = arrayCpuCore[spCpuCore.getSelectedItemPosition()].equals(CpuUtils.getIntValue(FileDefine.CPU_CORE));
        boolean ret3 = arrayDdrFreq[spDdrFreq.getSelectedItemPosition()].equals(CpuUtils.getIntValue(FileDefine.DDR_FREQ));
        boolean ret4 = arrayGpuFreq[spGpuFreq.getSelectedItemPosition()].equals(CpuUtils.getIntValue(FileDefine.GPU_FREQ));
        boolean applied = (ret1 && ret2 && ret3 & ret4);
        tvApply.setText(applied ? R.string.settings_applied : R.string.settings_not_applied);
        tvApply.setTextColor(applied ? Color.GREEN : Color.RED);
    }

    private void setChangedState() {
        boolean ret1 = arrayCpuFreq[spCpuFreq.getSelectedItemPosition()].equals(Config.getCpuFreq(this));
        boolean ret2 = arrayCpuCore[spCpuCore.getSelectedItemPosition()].equals(Config.getCpuCore(this));
        boolean ret3 = arrayDdrFreq[spDdrFreq.getSelectedItemPosition()].equals(Config.getDdrFreq(this));
        boolean ret4 = arrayGpuFreq[spGpuFreq.getSelectedItemPosition()].equals(Config.getGpuFreq(this));
        boolean keeped = (ret1 && ret2 && ret3 && ret4);
        if (!keeped) {
            tvApply.setText(R.string.settings_changed);
            tvApply.setTextColor(Color.YELLOW);
        } else {
            checkApplyState();
        }
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        menu.clear();
        itemSave = menu.add(0, MenuDefine.MENUID_SAVE, 99, R.string.save);
        itemSave.setIcon(android.R.drawable.ic_menu_save);
        itemSave.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
        itemReset = menu.add(0, MenuDefine.MENUID_RESET, 98, R.string.reset);
        itemReset.setIcon(android.R.drawable.ic_menu_revert);
        itemReset.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
        return super.onCreateOptionsMenu(menu);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case MenuDefine.MENUID_SAVE:
                saveData();
                break;
            case MenuDefine.MENUID_RESET:
                spProfile.setSelection(0);
                spCpuFreq.setSelection(0);
                spCpuCore.setSelection(0);
                spDdrFreq.setSelection(0);
                spGpuFreq.setSelection(0);
                saveData();
                break;
        }
        return true;
    }

    @Override
    public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
        if (parent.equals(spProfile)) {
            String[] sArr = getResources().getStringArray(profiles[position]);
            spCpuFreq.setSelection(CpuUtils.getStringIndex(arrayCpuFreq, sArr[0]));
            spCpuCore.setSelection(CpuUtils.getStringIndex(arrayCpuCore, sArr[1]));
            spDdrFreq.setSelection(CpuUtils.getStringIndex(arrayDdrFreq, sArr[2]));
            spGpuFreq.setSelection(CpuUtils.getStringIndex(arrayGpuFreq, sArr[3]));
        }
        setChangedState();
    }

    @Override
    public void onNothingSelected(AdapterView<?> parent) {

    }
}

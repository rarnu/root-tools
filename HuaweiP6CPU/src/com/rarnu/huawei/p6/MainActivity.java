package com.rarnu.huawei.p6;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.ArrayAdapter;
import android.widget.Spinner;
import android.widget.Toast;
import com.rarnu.command.RootUtils;
import com.rarnu.huawei.p6.common.FileDefine;
import com.rarnu.huawei.p6.common.MenuDefine;
import com.rarnu.huawei.p6.utils.CpuUtils;
import com.rarnu.huawei.p6.utils.DeviceCheckUtils;
import com.rarnu.utils.UIUtils;

public class MainActivity extends Activity {

    Spinner spCpuFreq, spCpuCore, spDdrFreq, spGpuFreq;
    ArrayAdapter<String> adapterCpuFreq, adapterCpuCore, adapterDdrFreq, adapterGpuFreq;
    String[] arrayCpuFreq, arrayCpuCore, arrayDdrFreq, arrayGpuFreq;
    MenuItem itemSave, itemReset;

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
        initData();
    }

    private void initComponents() {
        spCpuFreq = (Spinner) findViewById(R.id.spCpuFreq);
        spCpuCore = (Spinner) findViewById(R.id.spCpuCore);
        spDdrFreq = (Spinner) findViewById(R.id.spDdrFreq);
        spGpuFreq = (Spinner) findViewById(R.id.spGpuFreq);

        arrayCpuFreq = getResources().getStringArray(R.array.array_cpu_freq);
        arrayCpuCore = getResources().getStringArray(R.array.array_cpu_core);
        arrayDdrFreq = getResources().getStringArray(R.array.array_ddr_freq);
        arrayGpuFreq = getResources().getStringArray(R.array.array_gpu_freq);

        adapterCpuFreq = new ArrayAdapter<String>(this, R.layout.item, R.id.tvItem, arrayCpuFreq);
        adapterCpuCore = new ArrayAdapter<String>(this, R.layout.item, R.id.tvItem, arrayCpuCore);
        adapterDdrFreq = new ArrayAdapter<String>(this, R.layout.item, R.id.tvItem, arrayDdrFreq);
        adapterGpuFreq = new ArrayAdapter<String>(this, R.layout.item, R.id.tvItem, arrayGpuFreq);

        spCpuFreq.setAdapter(adapterCpuFreq);
        spCpuCore.setAdapter(adapterCpuCore);
        spDdrFreq.setAdapter(adapterDdrFreq);
        spGpuFreq.setAdapter(adapterGpuFreq);

    }

    private void initData() {
        spCpuFreq.setSelection(CpuUtils.getStringIndex(arrayCpuFreq, CpuUtils.getIntValue(FileDefine.CPU_FREQ)));
        spCpuCore.setSelection(CpuUtils.getStringIndex(arrayCpuCore, CpuUtils.getIntValue(FileDefine.CPU_CORE)));
        spDdrFreq.setSelection(CpuUtils.getStringIndex(arrayDdrFreq, CpuUtils.getIntValue(FileDefine.DDR_FREQ)));
        spGpuFreq.setSelection(CpuUtils.getStringIndex(arrayGpuFreq, CpuUtils.getIntValue(FileDefine.GPU_FREQ)));
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
                spCpuFreq.setSelection(0);
                spCpuCore.setSelection(0);
                spDdrFreq.setSelection(0);
                spGpuFreq.setSelection(0);
                saveData();
                break;
        }
        return true;
    }
}

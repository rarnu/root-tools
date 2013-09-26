package com.rarnu.tools.root.fragment;

import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.ListView;
import android.widget.Toast;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.DataProgressBar;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.HtcRomAdapter;
import com.rarnu.tools.root.common.Actions;
import com.rarnu.tools.root.common.HtcRomInfo;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.tools.root.common.RTConsts;
import com.rarnu.tools.root.receiver.MutaxReceiver;
import com.rarnu.tools.root.receiver.MutaxReceiver.OnReceiveMessage;
import com.rarnu.tools.root.service.HtcRomService;

import java.util.ArrayList;
import java.util.List;

public class HtcRomFragment extends BaseFragment implements OnReceiveMessage {

    ListView lstRomCleaner;
    DataProgressBar progressHtcRom;
    List<HtcRomInfo> list = null;
    HtcRomAdapter adapter = null;
    MutaxReceiver receiver;
    MenuItem itemClean;

    @Override
    public int getBarTitle() {
        return R.string.clean_htc_rom;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.clean_htc_rom_with_path;
    }

    @Override
    public void onResume() {
        super.onResume();
        receiver.register(getActivity());
    }

    @Override
    public void onPause() {
        receiver.unregister(getActivity());
        super.onPause();
    }

    @Override
    public void initComponents() {
        lstRomCleaner = (ListView) innerView.findViewById(R.id.lstRomCleaner);
        progressHtcRom = (DataProgressBar) innerView.findViewById(R.id.progressHtcRom);
        list = new ArrayList<HtcRomInfo>();
        adapter = new HtcRomAdapter(getActivity(), list);
        lstRomCleaner.setAdapter(adapter);
        receiver = new MutaxReceiver(Actions.ACTION_CLEANING_HTC, null, null);

    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_htcrom;
    }

    @Override
    public void initMenu(Menu menu) {
        itemClean = menu.add(0, MenuItemIds.MENU_CLEAN, 99, R.string.clean);
        itemClean.setIcon(android.R.drawable.ic_menu_delete);
        itemClean.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);

    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case MenuItemIds.MENU_CLEAN:
                cleanHtcRom();
                break;
        }
        return true;
    }

    private void cleanHtcRom() {
        if (getSelectedItemCount() == 0) {
            Toast.makeText(getActivity(), R.string.no_clean_item_selected, Toast.LENGTH_LONG).show();
            return;
        }

        new AlertDialog.Builder(getActivity())
                .setTitle(R.string.clean_htc_rom)
                .setMessage(R.string.clean_htc_rom_confirm)
                .setPositiveButton(R.string.ok, new DialogInterface.OnClickListener() {

                    @Override
                    public void onClick(DialogInterface dialog, int which) {
                        doCleanRom();

                    }
                })
                .setNegativeButton(R.string.cancel, null)
                .show();

    }

    private void doCleanRom() {
        setCleaningState(true);
        // clean service
        Intent inHtcRomService = new Intent(getActivity(), HtcRomService.class);
        inHtcRomService.putExtra("command", buildCommand());
        inHtcRomService.putExtra("id", RTConsts.NOTIFY_ID_HTC_ROM);
        inHtcRomService.putExtra("title", R.string.clean_htc_rom);
        inHtcRomService.putExtra("desc", R.string.clean_htc_rom_finish);
        inHtcRomService.putExtra("proc_id", RTConsts.NOTIFY_PROC_HTC_ROM);
        inHtcRomService.putExtra("proc_title", R.string.clean_htc_rom);
        inHtcRomService.putExtra("proc_desc", R.string.cleaning_proc);
        getActivity().startService(inHtcRomService);
    }

    private String buildCommand() {
        String cmd = "";

        for (int i = 0; i < list.size(); i++) {
            cmd += list.get(i).checked ? "1" : "0";
        }
        return cmd;
    }

    private void setAllItemUnchecked() {
        for (int i = 0; i < list.size(); i++) {
            list.get(i).checked = false;
        }
        adapter.setNewList(list);
    }

    private int getSelectedItemCount() {
        int ret = 0;
        for (int i = 0; i < list.size(); i++) {
            if (list.get(i).checked) {
                ret++;
            }
        }
        return ret;
    }

    private void setCleaningState(boolean cleaning) {
        try {
            progressHtcRom.setAppName(getString(R.string.cleaning_htcrom));
            progressHtcRom.setVisibility(cleaning ? View.VISIBLE : View.GONE);

            adapter.setCheckable(!cleaning);
            if (!cleaning) {
                setAllItemUnchecked();
            }
        } catch (Exception e) {

        }
    }

    @Override
    public void initLogic() {
        list.clear();
        list.add(buildRomInfo(R.string.itm_custom, R.string.itmdesc_custom));
        list.add(buildRomInfo(R.string.itm_car, R.string.itmdesc_car));
        list.add(buildRomInfo(R.string.itm_facebook, R.string.itmdesc_facebook));
        list.add(buildRomInfo(R.string.itm_twitter, R.string.itmdesc_twitter));
        list.add(buildRomInfo(R.string.itm_dropbox, R.string.itmdesc_dropbox));
        list.add(buildRomInfo(R.string.itm_skydrive, R.string.itmdesc_skydrive));
        list.add(buildRomInfo(R.string.itm_laputa, R.string.itmdesc_laputa));
        list.add(buildRomInfo(R.string.itm_flickr, R.string.itmdesc_flickr));
        list.add(buildRomInfo(R.string.itm_friendstream, R.string.itmdesc_friendstream));
        list.add(buildRomInfo(R.string.itm_google, R.string.itmdesc_google));
        list.add(buildRomInfo(R.string.itm_3rd, R.string.itmdesc_3rd));
        list.add(buildRomInfo(R.string.itm_cm3rd, R.string.itmdesc_cm3rd));

        adapter.setNewList(list);
        adapter.setCheckable(true);
    }

    private HtcRomInfo buildRomInfo(int resTitle, int resDesc) {
        HtcRomInfo info = new HtcRomInfo();
        info.title = getString(resTitle);
        info.desc = getString(resDesc);
        return info;
    }

    @Override
    public void onStateChange(boolean operating) {
        if (!operating) {
            Intent inHtcRomService = new Intent(getActivity(), HtcRomService.class);
            getActivity().stopService(inHtcRomService);
        }
        setCleaningState(operating);

    }

    @Override
    public void onProgress(String name, int position, int total) {

    }

    @Override
    public void onMutaxMessage(boolean operating) {

    }

    @Override
    public void initEvents() {
        receiver.setOnReceiveMessage(this);

    }

    @Override
    public String getMainActivityName() {
        return MainActivity.class.getName();
    }

    @Override
    public void onGetNewArguments(Bundle bn) {

    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public Bundle getFragmentState() {
        return null;
    }

}

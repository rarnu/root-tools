package com.rarnu.tools.root.fragment;

import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.EditText;
import android.widget.TextView;
import android.widget.Toast;
import com.rarnu.devlib.base.BasePopupFragment;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.tools.root.utils.DirHelper;
import com.rarnu.tools.root.utils.HostsUtils;
import com.rarnu.utils.FileUtils;

import java.io.File;
import java.util.List;

public class HostEditFragment extends BasePopupFragment {

    private static final String PATH_HOSTS = "/system/etc/hosts";
    private static final String LOCAL_HOSTS = DirHelper.HOSTS_DIR + "hosts";
    EditText etEditHosts;
    TextView tvTooBigHint;
    MenuItem itemSave = null;
    boolean canEdit = false;

    @Override
    public int getBarTitle() {
        return R.string.manual_edit_hosts;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.manual_edit_hosts;
    }

    @Override
    public void initComponents() {
        etEditHosts = (EditText) innerView.findViewById(R.id.etEditHosts);
        tvTooBigHint = (TextView) innerView.findViewById(R.id.tvTooBigHint);
    }

    @Override
    public void initLogic() {
        loadHosts();

    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_host_edit;
    }

    @Override
    public void initMenu(Menu menu) {
        itemSave = menu.add(0, MenuItemIds.MENU_SAVE, 99, R.string.save);
        itemSave.setIcon(android.R.drawable.ic_menu_save);
        itemSave.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
        itemSave.setEnabled(canEdit);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case MenuItemIds.MENU_SAVE:
                if (!saveHosts()) {
                    Toast.makeText(getActivity(), R.string.save_hosts_error, Toast.LENGTH_LONG).show();
                } else {
                    Toast.makeText(getActivity(), R.string.save_hosts_succ, Toast.LENGTH_LONG).show();
                    getActivity().finish();
                }
                break;
        }
        return true;
    }

    private void loadHosts() {

        File fHost = new File(PATH_HOSTS);
        canEdit = (fHost.length() <= 1024 * 10);

        tvTooBigHint.setVisibility(canEdit ? View.GONE : View.VISIBLE);
        etEditHosts.setEnabled(canEdit);
        if (itemSave != null) {
            itemSave.setEnabled(canEdit);
        }
        if (!canEdit) {
            return;
        }

        List<String> hosts = null;
        try {
            hosts = FileUtils.readFile(PATH_HOSTS);
            String hostsStr = "";
            if (hosts != null && hosts.size() != 0) {
                for (String s : hosts) {
                    hostsStr += s + "\n";
                }
            }
            etEditHosts.setText(hostsStr);
        } catch (Exception e) {
            etEditHosts.setText("");
        }
    }

    private boolean saveHosts() {

        String hosts = etEditHosts.getText().toString();
        return HostsUtils.copyHosts(hosts, LOCAL_HOSTS);

    }

    @Override
    public void initEvents() {

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

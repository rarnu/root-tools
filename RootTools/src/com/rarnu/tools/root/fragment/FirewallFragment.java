package com.rarnu.tools.root.fragment;

import android.content.Loader;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.ListView;
import android.widget.SearchView;
import android.widget.Toast;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.DataProgressBar;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.IptablesAdapter;
import com.rarnu.tools.root.common.IptablePackageInfo;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.tools.root.loader.FirewallLoader;
import com.rarnu.tools.root.utils.IptablesUtils;
import com.rarnu.utils.ImageUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

public class FirewallFragment extends BaseFragment implements Loader.OnLoadCompleteListener<List<IptablePackageInfo>>, SearchView.OnQueryTextListener {

    ListView lvFirewall;
    DataProgressBar progressFirewall;
    MenuItem miEnabled;
    MenuItem miRefresh;
    MenuItem miApply;
    SearchView sv;
    List<IptablePackageInfo> list;
    IptablesAdapter adapter = null;
    FirewallLoader loader;
    Comparator<IptablePackageInfo> comp = new Comparator<IptablePackageInfo>() {
        @Override
        public int compare(IptablePackageInfo o1, IptablePackageInfo o2) {
            if (o1.firstseem != o2.firstseem) {
                return (o1.firstseem ? -1 : 1);
            }
            if ((o1.selected_wifi | o1.selected_3g) == (o2.selected_wifi | o2.selected_3g)) {
                return String.CASE_INSENSITIVE_ORDER.compare(o1.names[0], o2.names[0]);
            }
            if (o1.selected_wifi || o1.selected_3g) return -1;
            return 1;
        }
    };

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
        lvFirewall = (ListView) innerView.findViewById(R.id.lvFirewall);
        progressFirewall = (DataProgressBar) innerView.findViewById(R.id.progressFirewall);
        loader = new FirewallLoader(getActivity());
        list = new ArrayList<IptablePackageInfo>();
        adapter = new IptablesAdapter(getActivity(), list);
        lvFirewall.setAdapter(adapter);
        sv = (SearchView) innerView.findViewById(R.id.sv);
    }

    @Override
    public void initEvents() {
        loader.registerListener(0, this);
        sv.setOnQueryTextListener(this);
    }

    @Override
    public void initLogic() {
        doStartLoad();
    }

    private void doStartLoad() {
        progressFirewall.setAppName(getString(R.string.loading));
        progressFirewall.setVisibility(View.VISIBLE);
        loader.startLoading();
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_firewall;
    }

    @Override
    public String getMainActivityName() {
        return MainActivity.class.getName();
    }

    @Override
    public void initMenu(Menu menu) {

        miEnabled = menu.add(0, MenuItemIds.MENU_ENABLED, 98, R.string.enable);
        miEnabled.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
        boolean enabled = IptablesUtils.isEnabled(getActivity());
        miEnabled.setIcon(ImageUtils.loadActionBarIcon(getActivity(), enabled ? R.drawable.firewall_enabled : R.drawable.firewall_disabled));

        miApply = menu.add(0, MenuItemIds.MENU_APPLY, 99, R.string.save);
        miApply.setIcon(android.R.drawable.ic_menu_save);
        miApply.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);

        miRefresh = menu.add(0, MenuItemIds.MENU_REFRESH, 100, R.string.refresh);
        miRefresh.setIcon(R.drawable.ic_menu_refresh);
        miRefresh.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);


    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case MenuItemIds.MENU_ENABLED:
                disableOrEnable();
                break;
            case MenuItemIds.MENU_APPLY:
                applyOrSaveRules();
                break;
            case MenuItemIds.MENU_REFRESH:
                doStartLoad();
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
    public void onLoadComplete(Loader<List<IptablePackageInfo>> loader, List<IptablePackageInfo> data) {
        list.clear();
        if (data != null) {
            list.addAll(data);
            Collections.sort(list, comp);
        }
        if (getActivity() != null) {
            adapter.setNewList(list);
            progressFirewall.setVisibility(View.GONE);
        }
    }

    private void disableOrEnable() {
        boolean enabled = !IptablesUtils.isEnabled(getActivity());
        IptablesUtils.setEnabled(getActivity(), enabled);
        if (enabled) {
            applyOrSaveRules();
            miEnabled.setIcon(ImageUtils.loadActionBarIcon(getActivity(), R.drawable.firewall_enabled));
        } else {
            purgeRules();
            miEnabled.setIcon(ImageUtils.loadActionBarIcon(getActivity(), R.drawable.firewall_disabled));
        }
    }

    private void applyOrSaveRules() {
        final boolean enabled = IptablesUtils.isEnabled(getActivity());

        if (enabled) {
            if (IptablesUtils.applyIptablesRules(getActivity())) {
                Toast.makeText(getActivity(), R.string.rules_applied, Toast.LENGTH_SHORT).show();
            } else {
                IptablesUtils.setEnabled(getActivity(), false);
                miEnabled.setIcon(ImageUtils.loadActionBarIcon(getActivity(), R.drawable.firewall_disabled));
            }
        } else {
            IptablesUtils.saveRules(getActivity());
            Toast.makeText(getActivity(), R.string.rules_saved, Toast.LENGTH_SHORT).show();
        }
    }

    private void purgeRules() {
        IptablesUtils.purgeIptables();
    }

    @Override
    public boolean onQueryTextChange(String newText) {
        if (adapter != null) {
            adapter.getFilter().filter(newText);
        }
        return true;
    }

    @Override
    public boolean onQueryTextSubmit(String query) {
        return false;
    }
}

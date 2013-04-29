package com.yugioh.android.fragments;

import java.util.ArrayList;
import java.util.List;

import android.content.Intent;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ArrayAdapter;
import android.widget.ListView;
import android.widget.Toast;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.utils.UIUtils;
import com.yugioh.android.AboutActivity;
import com.yugioh.android.MainActivity;
import com.yugioh.android.R;
import com.yugioh.android.UpdateActivity;
import com.yugioh.android.adapter.RightMenuAdapter;
import com.yugioh.android.classes.RightMenuItem;
import com.yugioh.android.classes.UpdateInfo;
import com.yugioh.android.intf.IMenuIntf;
import com.yugioh.android.utils.DeviceUtils;
import com.yugioh.android.utils.UpdateUtils;

public class RightMenuFragment extends BaseFragment implements IMenuIntf,
		OnItemClickListener {

	ListView lvAbout, lvSettings;
	List<String> listAbout;
	ArrayAdapter<String> adapterAbout;
	List<RightMenuItem> listSettings;
	RightMenuAdapter adapterSettings;
	UpdateInfo updateInfo;

	public RightMenuFragment(String tagText, String tabTitle) {
		super(tagText, tabTitle);
	}

	@Override
	public int getBarTitle() {
		return R.string.app_name;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.app_name;
	}

	@Override
	protected void initComponents() {
		lvSettings = (ListView) innerView.findViewById(R.id.lvSettings);
		lvAbout = (ListView) innerView.findViewById(R.id.lvAbout);
		listAbout = new ArrayList<String>();
		listAbout.add(getString(R.string.rm_about));
		adapterAbout = new ArrayAdapter<String>(getActivity(),
				R.layout.item_menu, listAbout);
		lvAbout.setAdapter(adapterAbout);

		listSettings = new ArrayList<RightMenuItem>();
		RightMenuItem itemUpdate = new RightMenuItem();
		itemUpdate.type = 0;
		itemUpdate.name = getString(R.string.rm_update);
		itemUpdate.value = 0;
		listSettings.add(itemUpdate);
		RightMenuItem itemFit = new RightMenuItem();
		itemFit.type = 1;
		itemFit.name = getString(R.string.rm_fitable);
		itemFit.value = DeviceUtils.getFitable(UIUtils.getDM());
		listSettings.add(itemFit);
		adapterSettings = new RightMenuAdapter(getActivity(), listSettings);
		lvSettings.setAdapter(adapterSettings);
	}

	@Override
	protected void initEvents() {
		lvAbout.setOnItemClickListener(this);
		lvSettings.setOnItemClickListener(this);
	}
	
	final Handler hUpdate = new Handler() {
		@Override
		public void handleMessage(Message msg) {
			if (msg.what == 1) {
				updateInfo = (UpdateInfo) msg.obj;
				updateMenu(updateInfo);
				
			}
			super.handleMessage(msg);
		}
	};

	@Override
	protected void initLogic() {
		UpdateUtils.checkUpdateT(getActivity(), hUpdate);
	}

	@Override
	protected int getFragmentLayoutResId() {
		return R.layout.menu_right;
	}

	@Override
	protected String getMainActivityName() {
		return MainActivity.class.getName();
	}

	@Override
	protected void initMenu(Menu menu) {

	}

	@Override
	protected void onGetNewArguments(Bundle bn) {

	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {
		switch (parent.getId()) {
		case R.id.lvSettings:
			switch (position) {
			case 0:
				if (updateInfo != null
						&& ((updateInfo.getUpdateApk() + updateInfo
								.getUpdateData()) != 0)) {
					Intent inUpdate = new Intent(getActivity(),
							UpdateActivity.class);
					inUpdate.putExtra("update", updateInfo);
					startActivity(inUpdate);
				} else {
					Toast.makeText(getActivity(), R.string.update_no,
							Toast.LENGTH_SHORT).show();
				}
				break;
			}
			break;
		case R.id.lvAbout:
			switch (position) {
			case 0:
				startActivity(new Intent(getActivity(), AboutActivity.class));
				break;
			}
			break;
		}
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void updateMenu(UpdateInfo updateInfo) {
		this.updateInfo = updateInfo;
		if (updateInfo != null) {
			listSettings.get(0).value = updateInfo.getUpdateApk()
					+ updateInfo.getUpdateData();
		}
		adapterSettings.setNewList(listSettings);

	}

}

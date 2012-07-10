package com.rarnu.tools.root;

import java.util.ArrayList;
import java.util.List;

import android.app.Activity;
import android.content.pm.PackageManager;
import android.content.pm.PackageParser;
import android.graphics.Color;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemLongClickListener;
import android.widget.ImageView;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;

import com.rarnu.tools.root.adapter.CompAdapter;
import com.rarnu.tools.root.api.LogApi;
import com.rarnu.tools.root.base.ActivityIntf;
import com.rarnu.tools.root.common.CompInfo;
import com.rarnu.tools.root.comp.TitleBar;
import com.rarnu.tools.root.utils.ComponentUtils;

public class CompPackageInfoActivity extends Activity implements ActivityIntf, OnItemLongClickListener, OnClickListener {

	// [region] field define
	TitleBar tbTitle;
	ImageView ivAppIcon;
	TextView tvAppName, tvAppPackage;
	ListView lvReceiver;
	// [/region]

	// [region] variable define
	CompAdapter adapter = null;
	List<CompInfo> lstComponentInfo = null;

	// [/region]

	// [region] life circle
	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.layout_comp_packageinfo);
		init();
		fillComponentList();

	}

	// [/region]

	// [region] init
	@Override
	public void init() {
		mappingComp();
		initTitle();
		initSearchBar();
		initEvents();
	}

	@Override
	public void mappingComp() {
		tbTitle = (TitleBar) findViewById(R.id.tbTitle);
		ivAppIcon = (ImageView) findViewById(R.id.ivAppIcon);
		tvAppName = (TextView) findViewById(R.id.tvAppName);
		tvAppPackage = (TextView) findViewById(R.id.tvAppPackage);
		lvReceiver = (ListView) findViewById(R.id.lvReceiver);

	}

	@Override
	public void initTitle() {
		tbTitle.getLeftButton().setVisibility(View.VISIBLE);
		tbTitle.setLeftButtonText(getString(R.string.back));
		tbTitle.setText(getString(R.string.component_list));

	}

	@Override
	public void initSearchBar() {

	}

	@Override
	public void initEvents() {
		lvReceiver.setOnItemLongClickListener(this);
		tbTitle.getLeftButton().setOnClickListener(this);
	}

	// [/region]

	// [region] business logic
	private void fillComponentList() {
		ivAppIcon.setBackgroundDrawable(GlobalInstance.pm
				.getApplicationIcon(GlobalInstance.currentComp.applicationInfo));
		tvAppName.setText(GlobalInstance.pm.getApplicationLabel(GlobalInstance.currentComp.applicationInfo));
		tvAppPackage.setText(GlobalInstance.currentComp.packageName);

		tvAppName
				.setTextColor(GlobalInstance.currentComp.applicationInfo.sourceDir.contains("/system/app/") ? Color.RED
						: Color.BLACK);

		PackageParser.Package pkg = ComponentUtils.parsePackageInfo(GlobalInstance.currentComp);
		if (pkg == null) {
			Toast.makeText(this, R.string.no_package_info_found, Toast.LENGTH_LONG).show();
			finish();
			return;
		}
		// lvReceiver
		lstComponentInfo = new ArrayList<CompInfo>();

		List<PackageParser.Activity> lstReceiver = pkg.receivers;
		for (PackageParser.Activity a : lstReceiver) {
			CompInfo info = new CompInfo();
			info.component = a;
			info.enabled = GlobalInstance.pm.getComponentEnabledSetting(a.getComponentName()) != PackageManager.COMPONENT_ENABLED_STATE_DISABLED;
			lstComponentInfo.add(info);
		}

		List<PackageParser.Service> lstService = pkg.services;
		for (PackageParser.Service s : lstService) {
			CompInfo info = new CompInfo();
			info.component = s;
			info.enabled = GlobalInstance.pm.getComponentEnabledSetting(s.getComponentName()) != PackageManager.COMPONENT_ENABLED_STATE_DISABLED;
			lstComponentInfo.add(info);
		}
		adapter = new CompAdapter(getLayoutInflater(), lstComponentInfo);
		lvReceiver.setAdapter(adapter);

	}

	// [/region]

	// [region] events
	@Override
	public boolean onItemLongClick(AdapterView<?> parent, View view, int position, long id) {
		CompInfo item = (CompInfo) lvReceiver.getItemAtPosition(position);
		boolean bRet = false;
		if (item.enabled) {
			// item.component.getComponentName()
			LogApi.logDisableComponent(item.component.getComponentName().toString());
			bRet = ComponentUtils.doDisableReceiver(item.component.getComponentName());
			if (bRet) {
				item.enabled = false;
				((TextView) view.findViewById(R.id.itemReceiverStatus)).setText(R.string.comp_disabled);
				((TextView) view.findViewById(R.id.itemReceiverStatus)).setTextColor(Color.RED);
			} else {
				Toast.makeText(this, R.string.operation_failed, Toast.LENGTH_LONG).show();
			}
		} else if (!item.enabled) {
			LogApi.logEnableComponent(item.component.getComponentName().toString());
			bRet = ComponentUtils.doEnabledReceiver(item.component.getComponentName());
			if (bRet) {
				item.enabled = true;
				((TextView) view.findViewById(R.id.itemReceiverStatus)).setText(R.string.comp_enabled);
				((TextView) view.findViewById(R.id.itemReceiverStatus)).setTextColor(0xFF008000);
			} else {
				Toast.makeText(this, R.string.operation_failed, Toast.LENGTH_LONG).show();
			}
		}
		setResult(RESULT_OK);
		return false;
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnLeft:
			finish();
			break;
		}

	}

	// [/region]
}

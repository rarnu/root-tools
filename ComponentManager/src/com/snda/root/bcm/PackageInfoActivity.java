package com.snda.root.bcm;

import java.util.ArrayList;
import java.util.List;

import android.app.Activity;
import android.content.pm.PackageManager;
import android.content.pm.PackageParser;
import android.graphics.Color;
import android.os.Bundle;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ImageView;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;
import android.widget.AdapterView.OnItemLongClickListener;

import com.snda.root.bcm.adapter.ComponentAdapter;
import com.snda.root.bcm.utils.ComponentUtils;

public class PackageInfoActivity extends Activity implements
		OnItemLongClickListener {

	ImageView ivAppIcon;
	TextView tvAppName, tvAppPackage;

	ListView lvReceiver;
	ComponentAdapter adapter = null;

	List<ComponentFullInfo> lstComponentInfo = null;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		setContentView(R.layout.packageinfo);

		ivAppIcon = (ImageView) findViewById(R.id.ivAppIcon);
		tvAppName = (TextView) findViewById(R.id.tvAppName);
		tvAppPackage = (TextView) findViewById(R.id.tvAppPackage);

		lvReceiver = (ListView) findViewById(R.id.lvReceiver);

		ivAppIcon.setBackgroundDrawable(GlobalInstance.currentPackageInfo.icon);
		tvAppName.setText(GlobalInstance.currentPackageInfo.label);
		tvAppPackage
				.setText(GlobalInstance.currentPackageInfo.pack.packageName);

		tvAppName
				.setTextColor(GlobalInstance.currentPackageInfo.isSytemApp ? Color.BLUE
						: Color.BLACK);

		lvReceiver.setOnItemLongClickListener(this);

		fillComponentList();

	}

	private void fillComponentList() {
		// lvReceiver
		lstComponentInfo = new ArrayList<ComponentFullInfo>();
		
		List<PackageParser.Activity> lstReceiver = GlobalInstance.currentPackageInfo.pack.receivers;
		for (PackageParser.Activity a : lstReceiver) {
			ComponentFullInfo info = new ComponentFullInfo();
			info.component = a;
			info.enabled = GlobalInstance.pm.getComponentEnabledSetting(a
					.getComponentName()) != PackageManager.COMPONENT_ENABLED_STATE_DISABLED;
			lstComponentInfo.add(info);
		}
		
		List<PackageParser.Service> lstService = GlobalInstance.currentPackageInfo.pack.services;
		for (PackageParser.Service s: lstService) {
			ComponentFullInfo info = new ComponentFullInfo();
			info.component = s;
			info.enabled = GlobalInstance.pm.getComponentEnabledSetting(s
					.getComponentName()) != PackageManager.COMPONENT_ENABLED_STATE_DISABLED;
			lstComponentInfo.add(info);
		}
		adapter = new ComponentAdapter(getLayoutInflater(), lstComponentInfo);
		lvReceiver.setAdapter(adapter);

	}

	@Override
	public boolean onItemLongClick(AdapterView<?> parent, View view,
			int position, long id) {
		ComponentFullInfo item = (ComponentFullInfo) lvReceiver
				.getItemAtPosition(position);
		boolean bRet = false;
		if (item.enabled) {
//			item.component.getComponentName()
			bRet = ComponentUtils.doDisableReceiver(item.component.getComponentName());
			if (bRet) {
				item.enabled = false;
				((TextView) view.findViewById(R.id.itemReceiverStatus)).setText(R.string.comp_disabled);
				((TextView) view.findViewById(R.id.itemReceiverStatus)).setTextColor(Color.RED);
			} else {
				Toast.makeText(this, R.string.operation_failed, Toast.LENGTH_LONG).show();
			}
		} else if (!item.enabled) {
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
}

package com.rarnu.tools.root;

import java.util.List;

import android.app.Fragment;
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
import com.rarnu.tools.root.base.BaseActivity;
import com.rarnu.tools.root.common.CompInfo;
import com.rarnu.tools.root.utils.ComponentUtils;

public class CompPackageInfoActivity extends BaseActivity implements
		OnItemLongClickListener, OnClickListener {

	// [region] field define

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

		fillComponentList();

	}

	// [/region]

	// [region] init
	

	
	public void mappingComp() {
		ivAppIcon = (ImageView) findViewById(R.id.ivAppIcon);
		tvAppName = (TextView) findViewById(R.id.tvAppName);
		tvAppPackage = (TextView) findViewById(R.id.tvAppPackage);
		lvReceiver = (ListView) findViewById(R.id.lvReceiver);

	}

	

	
	public void initEvents() {
		lvReceiver.setOnItemLongClickListener(this);

	}

	// [/region]

	// [region] business logic
	private void fillComponentList() {
		ivAppIcon
				.setBackgroundDrawable(GlobalInstance.pm
						.getApplicationIcon(GlobalInstance.currentComp.applicationInfo));
		tvAppName
				.setText(GlobalInstance.pm
						.getApplicationLabel(GlobalInstance.currentComp.applicationInfo));
		tvAppPackage.setText(GlobalInstance.currentComp.packageName);

		tvAppName
				.setTextColor(GlobalInstance.currentComp.applicationInfo.sourceDir
						.contains("/system/app/") ? Color.RED : Color.BLACK);

		Object /* PackageParser.Package */pkg = ComponentUtils
				.parsePackageInfo(GlobalInstance.currentComp);
		if (pkg == null) {
			Toast.makeText(this, R.string.no_package_info_found,
					Toast.LENGTH_LONG).show();
			finish();
			return;
		}
		// lvReceiver
		lstComponentInfo = ComponentUtils.getPackageRSList(pkg);
		adapter = new CompAdapter(getLayoutInflater(), lstComponentInfo);
		lvReceiver.setAdapter(adapter);

	}

	// [/region]

	// [region] events
	@Override
	public boolean onItemLongClick(AdapterView<?> parent, View view,
			int position, long id) {
		CompInfo item = (CompInfo) lvReceiver.getItemAtPosition(position);
		boolean bRet = false;
		if (item.enabled) {
			// item.component.getComponentName()
			LogApi.logDisableComponent(ComponentUtils.getPackageComponentName(
					item.component).toString());
			bRet = ComponentUtils.doDisableComponent(ComponentUtils
					.getPackageComponentName(item.component));
			if (bRet) {
				item.enabled = false;
				((TextView) view.findViewById(R.id.itemReceiverStatus))
						.setText(R.string.comp_disabled);
				((TextView) view.findViewById(R.id.itemReceiverStatus))
						.setTextColor(Color.RED);
			} else {
				Toast.makeText(this, R.string.operation_failed,
						Toast.LENGTH_LONG).show();
			}
		} else if (!item.enabled) {
			LogApi.logEnableComponent(ComponentUtils.getPackageComponentName(
					item.component).toString());
			bRet = ComponentUtils.doEnabledComponent(ComponentUtils
					.getPackageComponentName(item.component));
			if (bRet) {
				item.enabled = true;
				((TextView) view.findViewById(R.id.itemReceiverStatus))
						.setText(R.string.comp_enabled);
				((TextView) view.findViewById(R.id.itemReceiverStatus))
						.setTextColor(0xFF008000);
			} else {
				Toast.makeText(this, R.string.operation_failed,
						Toast.LENGTH_LONG).show();
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

	@Override
	public Fragment replaceFragment() {
		// TODO Auto-generated method stub
		return null;
	}

	// [/region]
}

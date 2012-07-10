package com.rarnu.tools.root;

import android.app.Activity;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.Window;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.TextView;
import android.widget.Toast;

import com.rarnu.tools.root.api.LogApi;
import com.rarnu.tools.root.base.ActivityIntf;
import com.rarnu.tools.root.utils.MemorySpecialList;
import com.rarnu.tools.root.utils.root.RootUtils;

public class MemProcessActivity extends Activity implements ActivityIntf, OnClickListener {

	// [region] field define
	ImageView ImgIcon;
	TextView tvName, tvNamespace, tvWarning;
	TextView tvPidValue, tvMemoryValue, tvUserValue;
	Button btnCancel, btnKill, btnIgnore;

	// [/region]
	
	// [region] life circle
	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		requestWindowFeature(Window.FEATURE_NO_TITLE);
		setContentView(R.layout.layout_mem_process);
		init();
		showProcessInfo();
		showIgnoreStatus();
	}
	// [/region]

	// [region] business logic
	private void showIgnoreStatus() {
		int inIgnore = MemorySpecialList.inExcludeList(GlobalInstance.currentMemoryProcess.NAME);
		btnIgnore.setText(inIgnore != -1 ? R.string.remove_ignore : R.string.add_ignore);
	}

	private void showProcessInfo() {
		if (GlobalInstance.currentMemoryProcess.appInfo == null) {
			ImgIcon.setBackgroundDrawable(getResources().getDrawable(R.drawable.android));
			tvName.setText(GlobalInstance.currentMemoryProcess.NAME);
			tvNamespace.setText("");
		} else {
			ImgIcon.setBackgroundDrawable(GlobalInstance.pm
					.getApplicationIcon(GlobalInstance.currentMemoryProcess.appInfo));
			tvName.setText(GlobalInstance.pm.getApplicationLabel(GlobalInstance.currentMemoryProcess.appInfo));
			tvNamespace.setText(GlobalInstance.currentMemoryProcess.NAME);
		}

		tvPidValue.setText(String.valueOf(GlobalInstance.currentMemoryProcess.PID));
		tvMemoryValue.setText(String.format("%dM", GlobalInstance.currentMemoryProcess.RSS));
		tvUserValue.setText(GlobalInstance.currentMemoryProcess.USER);

		tvWarning.setVisibility(GlobalInstance.currentMemoryProcess.USER.startsWith("app_") ? View.GONE : View.VISIBLE);

	}

	// [/region]
	
	// [region] events
	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnKill:
			if (MemorySpecialList.isExcludeLocked(GlobalInstance.currentMemoryProcess.NAME)) {
				Toast.makeText(this, R.string.locked_app_kill, Toast.LENGTH_LONG).show();
				return;
			}
			LogApi.logKillProcess(GlobalInstance.currentMemoryProcess.NAME);
			RootUtils.runCommand(String.format("kill %d", GlobalInstance.currentMemoryProcess.PID), true);
			setResult(RESULT_OK);
			finish();
			break;
		case R.id.btnCancel:
			finish();
			break;
		case R.id.btnIgnore:
			// add or remove ignore
			int inIgnore = MemorySpecialList.inExcludeList(GlobalInstance.currentMemoryProcess.NAME);
			if (inIgnore != -1) {
				// remove ignore
				if (MemorySpecialList.isExcludeLocked(GlobalInstance.currentMemoryProcess.NAME)) {
					Toast.makeText(this, R.string.locked_app_error, Toast.LENGTH_LONG).show();
				} else {
					LogApi.logUnignoreProcess(GlobalInstance.currentMemoryProcess.NAME);
					MemorySpecialList.removeExclude(GlobalInstance.currentMemoryProcess.NAME);
					if (MemorySpecialList.saveExclude()) {
						Toast.makeText(this, R.string.added_ignore, Toast.LENGTH_LONG).show();
					} else {
						Toast.makeText(this, R.string.added_ignore_error, Toast.LENGTH_LONG).show();
					}
				}
			} else {
				// add ignore
				LogApi.logIgnoreProcess(GlobalInstance.currentMemoryProcess.NAME);
				MemorySpecialList.addExclude(GlobalInstance.currentMemoryProcess.NAME);
				if (MemorySpecialList.saveExclude()) {
					Toast.makeText(this, R.string.added_ignore, Toast.LENGTH_LONG).show();
				} else {
					Toast.makeText(this, R.string.added_ignore_error, Toast.LENGTH_LONG).show();
				}
			}
			showIgnoreStatus();
			break;
		}
	}

	// [/region]
	
	// [region] init
	@Override
	public void init() {
		mappingComp();
		initSearchBar();
		initTitle();
		initEvents();

	}

	@Override
	public void mappingComp() {
		ImgIcon = (ImageView) findViewById(R.id.ImgIcon);
		tvName = (TextView) findViewById(R.id.tvName);
		tvNamespace = (TextView) findViewById(R.id.tvNamespace);
		tvWarning = (TextView) findViewById(R.id.tvWarning);
		tvPidValue = (TextView) findViewById(R.id.tvPidValue);
		tvMemoryValue = (TextView) findViewById(R.id.tvMemoryValue);
		tvUserValue = (TextView) findViewById(R.id.tvUserValue);
		btnCancel = (Button) findViewById(R.id.btnCancel);
		btnKill = (Button) findViewById(R.id.btnKill);
		btnIgnore = (Button) findViewById(R.id.btnIgnore);

	}

	@Override
	public void initTitle() {

	}

	@Override
	public void initSearchBar() {

	}

	@Override
	public void initEvents() {
		btnCancel.setOnClickListener(this);
		btnKill.setOnClickListener(this);
		btnIgnore.setOnClickListener(this);

	}
	
	// [/region]
}

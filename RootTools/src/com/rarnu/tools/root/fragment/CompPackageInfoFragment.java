package com.rarnu.tools.root.fragment;

import java.util.List;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.graphics.Color;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemLongClickListener;
import android.widget.ImageView;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;

import com.rarnu.devlib.base.BasePopupFragment;
import com.rarnu.devlib.utils.DrawableUtils;
import com.rarnu.devlib.utils.UIUtils;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.CompAdapter;
import com.rarnu.tools.root.common.CompInfo;
import com.rarnu.tools.root.utils.ComponentUtils;

public class CompPackageInfoFragment extends BasePopupFragment implements
		OnItemLongClickListener {

	ImageView ivAppIcon;
	TextView tvAppName, tvAppPackage;
	ListView lvReceiver;

	CompAdapter adapter = null;
	List<CompInfo> lstComponentInfo = null;

	@Override
	public int getBarTitle() {
		return R.string.component_list;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.component_list;
	}

	@Override
	protected void initComponents() {
		ivAppIcon = (ImageView) innerView.findViewById(R.id.ivAppIcon);
		tvAppName = (TextView) innerView.findViewById(R.id.tvAppName);
		tvAppPackage = (TextView) innerView.findViewById(R.id.tvAppPackage);
		lvReceiver = (ListView) innerView.findViewById(R.id.lvReceiver);

	}

	@Override
	protected void initLogic() {
		fillComponentList();

	}

	private void fillComponentList() {
		ivAppIcon
				.setBackgroundDrawable(GlobalInstance.pm
						.getApplicationIcon(GlobalInstance.currentComp.applicationInfo));
		tvAppName
				.setText(GlobalInstance.pm
						.getApplicationLabel(GlobalInstance.currentComp.applicationInfo));
		tvAppPackage.setText(GlobalInstance.currentComp.packageName);

		tvAppName.setTextColor(DrawableUtils.getTextColorPrimary(getActivity()));
		if (GlobalInstance.currentComp.applicationInfo.sourceDir
				.contains("/system/app/")) {
			tvAppName.setTextColor(Color.RED);
		}

		Object /* PackageParser.Package */pkg = ComponentUtils
				.parsePackageInfo(GlobalInstance.currentComp, UIUtils.getDM());
		if (pkg == null) {
			Toast.makeText(getActivity(), R.string.no_package_info_found,
					Toast.LENGTH_LONG).show();
			getActivity().finish();
			return;
		}
		// lvReceiver
		lstComponentInfo = ComponentUtils.getPackageRSList(pkg);
		adapter = new CompAdapter(getActivity(), lstComponentInfo);
		lvReceiver.setAdapter(adapter);

	}

	@Override
	protected int getFragmentLayoutResId() {
		return R.layout.layout_comp_packageinfo;
	}

	@Override
	protected void initMenu(Menu menu) {

	}

	@Override
	public boolean onItemLongClick(AdapterView<?> parent, final View view,
			int position, long id) {
		final CompInfo item = (CompInfo) lvReceiver.getItemAtPosition(position);
		if (item.enabled) {
			if (item.isServiceRunning(getActivity())) {
				new AlertDialog.Builder(getActivity())
						.setTitle(R.string.hint)
						.setMessage(R.string.service_is_running)
						.setPositiveButton(R.string.ok,
								new DialogInterface.OnClickListener() {
									@Override
									public void onClick(DialogInterface dialog,
											int which) {
										doDisableComponent(item, view);
									}
								}).setNegativeButton(R.string.cancel, null)
						.show();
			} else {
				doDisableComponent(item, view);
			}
		} else if (!item.enabled) {
			doEnableComponent(item, view);
		}
		getActivity().setResult(Activity.RESULT_OK);
		return false;
	}

	private void doEnableComponent(CompInfo item, View view) {
		boolean bRet = ComponentUtils.doEnabledComponent(ComponentUtils
				.getPackageComponentName(item.component));
		if (bRet) {
			item.enabled = true;
			((TextView) view.findViewById(R.id.itemReceiverStatus))
					.setText(R.string.comp_enabled);
			((TextView) view.findViewById(R.id.itemReceiverStatus))
					.setTextColor(Color.GREEN);
			adapter.notifyDataSetChanged();
		} else {
			Toast.makeText(getActivity(), R.string.operation_failed,
					Toast.LENGTH_LONG).show();
		}
	}

	private void doDisableComponent(CompInfo item, View view) {
		boolean bRet = ComponentUtils.doDisableComponent(ComponentUtils
				.getPackageComponentName(item.component));
		if (bRet) {
			item.enabled = false;
			((TextView) view.findViewById(R.id.itemReceiverStatus))
					.setText(R.string.comp_disabled);
			((TextView) view.findViewById(R.id.itemReceiverStatus))
					.setTextColor(Color.RED);
			adapter.notifyDataSetChanged();
		} else {
			Toast.makeText(getActivity(), R.string.operation_failed,
					Toast.LENGTH_LONG).show();
		}
	}

	@Override
	protected void initEvents() {
		lvReceiver.setOnItemLongClickListener(this);
	}

	@Override
	protected String getMainActivityName() {
		return MainActivity.class.getName();
	}

	@Override
	protected void onGetNewArguments(Bundle bn) {

	}

	@Override
	public String getCustomTitle() {
		return null;
	}

}

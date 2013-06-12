package com.zoe.calendar.fragment;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import android.os.Bundle;
import android.text.Html;
import android.view.Menu;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.GridView;
import android.widget.TextView;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.FileUtils;
import com.rarnu.utils.UIUtils;
import com.zoe.calendar.Global;
import com.zoe.calendar.R;
import com.zoe.calendar.adapter.SettingsTypeAdapter;
import com.zoe.calendar.classes.SettingTypeItem;
import com.zoe.calendar.common.Config;
import com.zoe.calendar.utils.ResourceUtils;

public class SettingsFragment extends BaseFragment implements
		OnItemClickListener {

	SettingsTypeAdapter adapterType;
	List<SettingTypeItem> listType;
	GridView gvType;
	TextView tvAboutValue;
	
	public SettingsFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.fragment_settings);
	}
	
	public SettingsFragment(String tag) {
		super(tag, "");
	}

	@Override
	public int getBarTitle() {
		return R.string.settings_name;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.settings_name;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {
		gvType = (GridView) innerView.findViewById(R.id.gvType);
		tvAboutValue = (TextView) innerView.findViewById(R.id.tvAboutValue);
		listType = new ArrayList<SettingTypeItem>();

		String[] strTypes = getResources().getStringArray(
				R.array.settings_types);

		for (int i = 0; i < Global.settingTypes.length; i++) {
			listType.add(new SettingTypeItem(strTypes[i],
					Global.settingTypes[i]));
		}

		adapterType = new SettingsTypeAdapter(getActivity(), listType);
		gvType.setNumColumns(3);
		gvType.setAdapter(adapterType);
		UIUtils.makeGridViewFullSize(gvType, UIUtils.dipToPx(56), 3);
	}

	@Override
	public void initEvents() {
		gvType.setOnItemClickListener(this);
	}

	@Override
	public void initLogic() {
		try {
			String aboutText = FileUtils.readAssetFile(getActivity(), "about");
			aboutText = aboutText.substring(0, aboutText.indexOf("==END=="));
			tvAboutValue.setText(Html.fromHtml(aboutText));
		} catch (IOException e) {

		}
	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_settings;
	}

	@Override
	public String getMainActivityName() {
		return "";
	}

	@Override
	public void initMenu(Menu menu) {

	}

	@Override
	public void onGetNewArguments(Bundle bn) {

	}

	@Override
	public Bundle getFragmentState() {
		return null;
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {
		boolean checked = listType.get(position).checked;
		checked = !checked;
		listType.get(position).checked = checked;
		Global.settingTypes[position] = checked;
		Config.setSettingType(getActivity(), position, checked);
		adapterType.notifyDataSetChanged();

		Global.filteredTagsString = Config.loadFiltedString(getActivity());
	}

}

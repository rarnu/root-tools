package com.zoe.calendar.fragment;

import java.util.ArrayList;
import java.util.List;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.Button;
import android.widget.GridView;

import com.rarnu.devlib.base.BaseDialogFragment;
import com.rarnu.utils.UIUtils;
import com.zoe.calendar.R;
import com.zoe.calendar.adapter.TagsTypeAdapter;
import com.zoe.calendar.utils.ResourceUtils;

public class TagDialogFrament extends BaseDialogFragment implements
		OnClickListener, OnItemClickListener {

	GridView gvTags;
	Button btnCancel;
	TagsTypeAdapter adapter;
	List<String> listType;

	public TagDialogFrament() {
		super();
		// tagText = ResourceUtils.getString(R.tag.fragment_tag_dialog);
	}

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
		gvTags = (GridView) innerView.findViewById(R.id.gvTags);
		btnCancel = (Button) innerView.findViewById(R.id.btnCancel);

		String[] strTypes = getResources().getStringArray(
				R.array.settings_types);
		listType = new ArrayList<String>();
		for (String s : strTypes) {
			listType.add(s);
		}
		adapter = new TagsTypeAdapter(getActivity(), listType);
		gvTags.setNumColumns(3);
		gvTags.setAdapter(adapter);
		UIUtils.makeGridViewFullSize(gvTags, UIUtils.dipToPx(56), 3);
	}

	@Override
	public void initEvents() {
		btnCancel.setOnClickListener(this);
		gvTags.setOnItemClickListener(this);
	}

	@Override
	public void initLogic() {

	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.dialog_select_tag;
	}

	@Override
	public String getMainActivityName() {
		return null;
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
	public void onClick(View v) {
		getActivity().finish();

	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {
		String tag = listType.get(position);
		Intent inResult = new Intent();
		inResult.putExtra("tag", tag);
		getActivity().setResult(Activity.RESULT_OK, inResult);
		getActivity().finish();

	}

}

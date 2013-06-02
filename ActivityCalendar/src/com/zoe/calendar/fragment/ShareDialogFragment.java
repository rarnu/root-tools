package com.zoe.calendar.fragment;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.Button;
import android.widget.ListView;
import android.widget.Toast;

import com.rarnu.devlib.base.BaseDialogFragment;
import com.rarnu.utils.UIUtils;
import com.zoe.calendar.Global;
import com.zoe.calendar.R;
import com.zoe.calendar.adapter.ShareAdapter;
import com.zoe.calendar.classes.ActivityItem;
import com.zoe.calendar.classes.ShareItem;

public class ShareDialogFragment extends BaseDialogFragment implements
		OnClickListener, OnItemClickListener {

	ListView lvShare;
	Button btnBack;
	ShareAdapter adapter;
	List<ShareItem> list;
	ActivityItem actItem;

	public ShareDialogFragment(String tag) {
		super(tag);
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
		lvShare = (ListView) innerView.findViewById(R.id.lvShare);
		btnBack = (Button) innerView.findViewById(R.id.btnBack);

		list = new ArrayList<ShareItem>();
		String[] titles = getResources().getStringArray(R.array.share_titles);
		String[] texts = getResources().getStringArray(R.array.share_texts);
		String[] packages = getResources().getStringArray(
				R.array.share_pacakges);
		String[] classes = getResources().getStringArray(R.array.share_classes);
		for (int i = 0; i < titles.length; i++) {
			list.add(new ShareItem(getResources().getIdentifier(
					String.format("share_icon_%d", i + 1), "drawable",
					getActivity().getPackageName()), titles[i], texts[i],
					packages[i], classes[i]));
		}
		adapter = new ShareAdapter(getActivity(), list);
		lvShare.setAdapter(adapter);
		UIUtils.makeListViewFullSize(lvShare, UIUtils.dipToPx(48));
	}

	@Override
	public void initEvents() {
		btnBack.setOnClickListener(this);
		lvShare.setOnItemClickListener(this);
	}

	@Override
	public void initLogic() {

	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.dialog_share;
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
		ShareItem item = list.get(position);
		actItem = (ActivityItem) getActivity().getIntent()
				.getSerializableExtra("item");
		Intent inShare = new Intent(Intent.ACTION_SEND);
		inShare.setType("image/*");

		inShare.putExtra(Intent.EXTRA_STREAM,
				Uri.fromFile(new File(Global.iconFilePath)));
		inShare.putExtra(Intent.EXTRA_TEXT, item.text + actItem.url);
		inShare.setPackage(item.packageName);
		if ((item.className != null) && (!item.className.equals(""))) {
			inShare.setClassName(item.packageName, item.className);
		}
		try {
			startActivity(inShare);
			getActivity().finish();
		} catch (Exception e) {
			Toast.makeText(getActivity(),
					getString(R.string.share_not_install, item.title),
					Toast.LENGTH_LONG).show();
		}

	}
}

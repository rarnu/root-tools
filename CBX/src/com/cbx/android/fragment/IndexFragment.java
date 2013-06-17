package com.cbx.android.fragment;

import java.util.List;

import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ListView;

import com.cbx.android.MainActivity;
import com.cbx.android.R;
import com.cbx.android.classes.DetailItem;
import com.cbx.android.utils.ResourceUtils;
import com.rarnu.devlib.base.BaseFragment;

public class IndexFragment extends BaseFragment implements OnItemClickListener {

	ListView lvIndex;
	List<DetailItem> list;

	public IndexFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.fragment_index);
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
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {
		lvIndex = (ListView) innerView.findViewById(R.id.lvIndex);
	}

	@Override
	public void initEvents() {
		lvIndex.setOnItemClickListener(this);
	}

	@Override
	public void initLogic() {

	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_index;
	}

	@Override
	public String getMainActivityName() {
		return MainActivity.class.getName();
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
		DetailItem item = (DetailItem) lvIndex.getItemAtPosition(position);
		BaseFragment bf = (BaseFragment) getFragmentManager()
				.findFragmentByTag(getString(R.tag.fragment_detail));
		if (bf != null) {
			Bundle bn = new Bundle();
			bn.putSerializable("item", item);
			bf.setNewArguments(bn);
		}
	}

}

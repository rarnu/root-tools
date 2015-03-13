package com.sbbs.me.android.adapter;

import java.util.List;

import com.rarnu.devlib.adapter.BaseAdapter;
import org.eclipse.egit.github.core.TreeEntry;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;
import com.sbbs.me.android.R;

public class SbbsMeTreeEntryAdapter extends BaseAdapter<TreeEntry> {

	public SbbsMeTreeEntryAdapter(Context context, List<TreeEntry> list) {
		super(context, list);
	}
	
	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		View v = convertView;
		if (v == null) {
			v = inflater.inflate(R.layout.item_entry, parent, false);
		}
		SbbsMeTreeEntryHolder holder = (SbbsMeTreeEntryHolder) v.getTag();
		if (holder == null) {
			holder = new SbbsMeTreeEntryHolder();
			holder.entryName = (TextView) v.findViewById(R.id.entryName);
			v.setTag(holder);
		}
		TreeEntry item = list.get(position);
		if (item != null) {
			holder.entryName.setText(item.getPath());
		}
		return v;
	}

	@Override
	public String getValueText(TreeEntry item) {
		return null;
	}

}

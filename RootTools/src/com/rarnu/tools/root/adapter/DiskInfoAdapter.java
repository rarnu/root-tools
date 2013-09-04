package com.rarnu.tools.root.adapter;

import java.util.List;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.DiskInfo;
import com.rarnu.tools.root.holder.DiskInfoHolder;

public class DiskInfoAdapter extends BaseAdapter<DiskInfo> {

	public DiskInfoAdapter(Context context, List<DiskInfo> list) {
		super(context, list);
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		View v = convertView;
		if (v == null) {
			v = inflater.inflate(R.layout.diskinfo_item, parent, false);
		}
		DiskInfoHolder holder = (DiskInfoHolder) v.getTag();
		if (holder == null) {
			holder = new DiskInfoHolder();
			holder.tvFileSystem = (TextView) v.findViewById(R.id.tvFileSystem);
			v.setTag(holder);
		}
		DiskInfo item = list.get(position);
		if (item != null) {
			holder.tvFileSystem.setText(item.fileSystem);
		}
		return v;
	}

	@Override
	public String getValueText(DiskInfo item) {
		return "";
	}

}

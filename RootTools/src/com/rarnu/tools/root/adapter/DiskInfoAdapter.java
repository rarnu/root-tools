package com.rarnu.tools.root.adapter;

import java.util.List;

import android.content.Context;
import android.text.Html;
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
			holder.tvTotal = (TextView) v.findViewById(R.id.tvTotal);
			holder.tvUsed = (TextView) v.findViewById(R.id.tvUsed);
			holder.tvFree = (TextView) v.findViewById(R.id.tvFree);
			holder.tvBlock = (TextView) v.findViewById(R.id.tvBlock);
			v.setTag(holder);
		}

		DiskInfo item = list.get(position);
		if (item != null) {
			holder.tvFileSystem.setText(item.fileSystem);
			holder.tvTotal.setText(context.getString(R.string.disk_size_fmt,
					item.size));
			holder.tvUsed.setText(Html.fromHtml(context.getString(
					R.string.disk_used_fmt, item.getColoredSize(0))));
			holder.tvFree.setText(Html.fromHtml(context.getString(
					R.string.disk_free_fmt, item.getColoredSize(1))));
			holder.tvBlock.setText(context.getString(R.string.disk_block_fmt,
					item.blockSize));
		}
		return v;
	}

	@Override
	public String getValueText(DiskInfo item) {
		return "";
	}

}

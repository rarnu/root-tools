package com.snda.root.hosts.adapter;

import java.util.List;
import java.util.Map;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.View.OnClickListener;
import android.widget.BaseAdapter;
import android.widget.CheckBox;
import android.widget.TextView;

import com.snda.root.hosts.R;

public class HostItemAdapter extends BaseAdapter {

	private LayoutInflater mInflater;
	private List<Map<String, String>> list;
	private boolean showCheckBox;

	public HostItemAdapter(Context context, List<Map<String, String>> list,
			boolean showCheckBox) {
		this.mInflater = LayoutInflater.from(context);
		this.list = list;
		this.showCheckBox = showCheckBox;
	}

	@Override
	public int getCount() {
		return list.size();
	}

	@Override
	public Object getItem(int position) {
		return list.get(position);
	}

	@Override
	public long getItemId(int position) {
		return position;
	}

	@Override
	public View getView(final int position, View convertView, ViewGroup parent) {
		final Map<String, String> item = list.get(position);

		View v;
		if (convertView != null) {
			v = convertView;
		} else {
			v = mInflater.inflate(R.layout.host_item, parent, false);
		}
		ItemAdapterHolder holder = (ItemAdapterHolder) v.getTag();
		if (holder == null) {
			holder = new ItemAdapterHolder();
			holder.tvItemIP = (TextView) v.findViewById(R.id.tvItem_IP);
			holder.tvItemDomain = (TextView) v.findViewById(R.id.tvItem_Domain);
			holder.chkItemChecked = (CheckBox) v
					.findViewById(R.id.chkItem_Checked);
			v.setTag(holder);
		}

		if (item != null) {
			holder.tvItemIP.setText(item.get("IP"));
			holder.tvItemDomain.setText(item.get("DOMAIN"));
			holder.chkItemChecked.setVisibility(showCheckBox ? View.VISIBLE
					: View.GONE);
			holder.chkItemChecked
					.setChecked(item.get("CHECKED").equals("true"));
			holder.chkItemChecked.setOnClickListener(new OnClickListener() {

				@Override
				public void onClick(View v) {
					item.put("CHECKED", ((CheckBox) v).isChecked() ? "true"
							: "false");

				}
			});
		}

		return v;
	}
}

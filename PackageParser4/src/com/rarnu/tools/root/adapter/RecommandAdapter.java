package com.rarnu.tools.root.adapter;

import java.util.List;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.TextView;

import com.rarnu.root.pp4.R;
import com.rarnu.tools.root.api.MobileApi;
import com.rarnu.tools.root.common.RecommandInfo;
import com.rarnu.tools.root.holder.RecommandAdapterHolder;
import com.rarnu.tools.root.utils.DownloadUtils;

public class RecommandAdapter extends BaseAdapter {

	private LayoutInflater inflater;
	private Context context;
	private List<RecommandInfo> list;

	public RecommandAdapter(Context context, List<RecommandInfo> list) {
		this.context = context;
		this.list = list;
		this.inflater = LayoutInflater.from(context);
	}

	public void setNewData(List<RecommandInfo> list) {
		this.list = list;
		this.notifyDataSetChanged();
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
	public View getView(int position, View convertView, ViewGroup parent) {
		View v = convertView;
		if (v == null) {
			v = inflater.inflate(R.layout.recommand_item, parent, false);
		}
		RecommandAdapterHolder holder = (RecommandAdapterHolder) v.getTag();
		if (holder == null) {
			holder = new RecommandAdapterHolder();
			holder.imgItemRecommand = (ImageView) v
					.findViewById(R.id.imgItemRecommand);
			holder.tvItemRecommand = (TextView) v
					.findViewById(R.id.tvItemRecommand);
			v.setTag(holder);
		}

		RecommandInfo item = list.get(position);
		if (item != null) {
			holder.tvItemRecommand.setText(item.name);
			DownloadUtils.downloadFileT(context, holder.imgItemRecommand,
					MobileApi.ICON_BASE_URL + item.iconUrl, item.unixName);
		}

		return v;
	}

}

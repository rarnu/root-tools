package com.rarnu.tools.root.adapter;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;
import com.rarnu.devlib.adapter.BaseAdapter;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.TeamBuildInfo;
import com.rarnu.tools.root.holder.TeamBuildAdapterHolder;

import java.util.List;

public class TeamBuildAdapter extends BaseAdapter<TeamBuildInfo> {
    public TeamBuildAdapter(Context context, List<TeamBuildInfo> list) {
        super(context, list);
    }

    @Override
    public String getValueText(TeamBuildInfo item) {
        return "";
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View v = convertView;
        if (v == null) {
            v = inflater.inflate(R.layout.team_build_item, parent, false);
        }
        TeamBuildAdapterHolder holder = (TeamBuildAdapterHolder) v.getTag();
        if (holder == null) {
            holder = new TeamBuildAdapterHolder();
            holder.tvName = (TextView) v.findViewById(R.id.tvName);
            v.setTag(holder);
        }
        TeamBuildInfo item = list.get(position);
        if (item != null) {
            holder.tvName.setText(item.title);

        }
        return v;
    }
}

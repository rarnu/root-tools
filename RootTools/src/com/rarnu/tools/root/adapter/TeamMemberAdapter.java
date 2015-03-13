package com.rarnu.tools.root.adapter;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;
import com.rarnu.devlib.adapter.BaseAdapter;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.api.MobileApi;
import com.rarnu.tools.root.common.TeamMemberInfo;
import com.rarnu.tools.root.holder.TeamMemberAdapterHolder;
import com.rarnu.tools.root.utils.DirHelper;
import com.rarnu.utils.DownloadUtils;

import java.util.List;

public class TeamMemberAdapter extends BaseAdapter<TeamMemberInfo> {
    public TeamMemberAdapter(Context context, List<TeamMemberInfo> list) {
        super(context, list);
    }

    @Override
    public String getValueText(TeamMemberInfo item) {
        return "";
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View v = convertView;
        if (v == null) {
            v = inflater.inflate(R.layout.team_member_item, parent, false);
        }
        TeamMemberAdapterHolder holder = (TeamMemberAdapterHolder) v.getTag();
        if (holder == null) {
            holder = new TeamMemberAdapterHolder();
            holder.tvName = (TextView) v.findViewById(R.id.tvName);
            holder.imgHead = (ImageView) v.findViewById(R.id.imgHead);
            holder.tvPosition = (TextView) v.findViewById(R.id.tvPosition);
            v.setTag(holder);
        }
        TeamMemberInfo item = list.get(position);
        if (item != null) {
            holder.tvName.setText(item.name);
            holder.tvPosition.setText(item.position);
            DownloadUtils.downloadFileT(context, holder.imgHead, MobileApi.MEMBER_HEAD_URL + item.headUrl, DirHelper.TEMP_DIR, item.headUrl, null);
        }
        return v;
    }
}

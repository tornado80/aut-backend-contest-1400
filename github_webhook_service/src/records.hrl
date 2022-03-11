%%%-------------------------------------------------------------------
%%% @author amirhosein
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Mar 2022 17:19
%%%-------------------------------------------------------------------
-author("amirhosein").

-record(submission, {
    repository,
    team,
    score
}).

-record(submission_team, {
    team_technology,
    team_id,
    team_name
}).

-record(submission_repository, {
    delivery_guid,
    repository_full_name,
    repository_name,
    repository_clone_url,
    repository_owner_login,
    head_commit_id,
    head_commit_message,
    head_commit_timestamp, % string
    pushed_at_timestamp % integer
}).
function ghreviews --wraps=gh\ search\ prs\ --state=open\ --review-requested=@me\ --json=repository,number,createdAt\ --sort=created\ --order=asc\ --template=\'\{\{range\ .\}\}\{\{tablerow\ \(printf\ \"https://github.com/\%s/pull/\%v\ \%s\"\ .repository.nameWithOwner\ .number\ .createdAt\ \|\ autocolor\ \"green\"\)\ .title\}\}\{\{end\}\}\' --description alias\ ghreviews=gh\ search\ prs\ --state=open\ --review-requested=@me\ --json=repository,number,createdAt\ --sort=created\ --order=asc\ --template=\'\{\{range\ .\}\}\{\{tablerow\ \(printf\ \"https://github.com/\%s/pull/\%v\ \%s\"\ .repository.nameWithOwner\ .number\ .createdAt\ \|\ autocolor\ \"green\"\)\ .title\}\}\{\{end\}\}\'
  gh search prs --state=open --review-requested=@me --json=repository,number,createdAt --sort=created --order=asc --template='{{range .}}{{tablerow (printf "https://github.com/%s/pull/%v %s" .repository.nameWithOwner .number .createdAt | autocolor "green") .title}}{{end}}' $argv; 
end

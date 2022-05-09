df <- create_raw_data(files)
df2 <- pivot_game_data(df, 'melondonkey')
summary_table <- create_summary_by_pokemon(df2)

create_analytic_oppmons(df2)